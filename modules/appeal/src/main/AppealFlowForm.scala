package lila.appeal

import play.api.data.*
import play.api.data.Forms.*

object AppealFlowForm:

  case class BranchData(id: String, answer: String, next: String)
  case class NodeData(
      id: String,
      kind: String,
      answerer: String,
      question: String,
      branches: List[BranchData],
      text: String,
      close: Boolean,
      unmark: Boolean,
      sleepMonths: Option[Int]
  )
  case class Data(rootId: String, nodes: List[NodeData])

  enum Edit:
    case Save
    case AddChoice
    case AddAction
    case AddBranch(index: Int)
    case RemoveNode(index: Int)
    case RemoveBranch(node: Int, branch: Int)
    case MoveNode(index: Int, delta: Int)

  private val branchMapping = mapping(
    "id" -> default(text, ""),
    "answer" -> default(text, ""),
    "next" -> default(text, "")
  )(BranchData.apply)(unapply)

  private val nodeMapping = mapping(
    "id" -> default(text, ""),
    "kind" -> default(text, "choice"),
    "answerer" -> default(text, "user"),
    "question" -> default(text, ""),
    "branches" -> list(branchMapping),
    "text" -> default(text, ""),
    "close" -> default(boolean, false),
    "unmark" -> default(boolean, false),
    "sleepMonths" -> optional(number(min = 1, max = 60))
  )(NodeData.apply)(unapply)

  val form = Form:
    mapping(
      "rootId" -> default(text, ""),
      "nodes" -> list(nodeMapping)
    )(Data.apply)(unapply)

  def from(flow: AppealFlow): Data =
    Data(
      rootId = flow.rootId.value,
      nodes = flow.nodes.map:
        case cn: ChoiceNode =>
          NodeData(
            id = cn.id.value,
            kind = "choice",
            answerer = cn.answerer match
              case Answerer.User => "user"
              case Answerer.Mod => "mod",
            question = cn.question,
            branches = cn.branches.toList.map: branch =>
              BranchData(branch.id.value, branch.answer, branch.nextNodeId.value),
            text = "",
            close = false,
            unmark = false,
            sleepMonths = none
          )
        case an: ActionNode =>
          NodeData(
            id = an.id.value,
            kind = "action",
            answerer = "user",
            question = "",
            branches = Nil,
            text = an.text,
            close = an.effects.contains(AppealEffect.Close),
            unmark = an.effects.contains(AppealEffect.Unmark),
            sleepMonths = an.effects.collectFirst { case AppealEffect.Sleep(months) => months }
          )
    )

  def readEdit(body: Map[String, Seq[String]]): Edit =
    def head(key: String) = body.get(key).flatMap(_.headOption).filter(_.nonEmpty)
    head("addBranch")
      .flatMap(_.toIntOption)
      .map(Edit.AddBranch.apply)
      .orElse(head("removeNode").flatMap(_.toIntOption).map(Edit.RemoveNode.apply))
      .orElse(head("removeBranch").flatMap(parsePair).map(Edit.RemoveBranch.apply))
      .orElse(head("moveNode").flatMap(parsePair).map(Edit.MoveNode.apply))
      .orElse(head("addChoice").as(Edit.AddChoice))
      .orElse(head("addAction").as(Edit.AddAction))
      .getOrElse(Edit.Save)

  def applyEdit(data: Data, edit: Edit): Data = edit match
    case Edit.Save => data
    case Edit.AddChoice => data.copy(nodes = data.nodes :+ blank("choice"))
    case Edit.AddAction => data.copy(nodes = data.nodes :+ blank("action"))
    case Edit.AddBranch(i) =>
      data.nodes
        .lift(i)
        .fold(data): node =>
          data.copy(nodes =
            data.nodes.updated(i, node.copy(branches = node.branches :+ BranchData("", "", "")))
          )
    case Edit.RemoveNode(i) if data.nodes.sizeIs > 1 =>
      data.copy(nodes = data.nodes.patch(i, Nil, 1))
    case Edit.RemoveBranch(i, j) =>
      data.nodes
        .lift(i)
        .fold(data): node =>
          data.copy(nodes = data.nodes.updated(i, node.copy(branches = node.branches.patch(j, Nil, 1))))
    case Edit.MoveNode(i, delta) =>
      val j = i + delta
      if data.nodes.indices.contains(i) && data.nodes.indices.contains(j) then
        val nodes = data.nodes.toArray
        val moved = nodes(i)
        nodes(i) = nodes(j)
        nodes(j) = moved
        data.copy(nodes = nodes.toList)
      else data
    case _ => data

  def build(data: Data): Either[List[String], AppealFlow] =
    val parsed = data.nodes.zipWithIndex.map(buildNode)
    val nodeErrors = parsed.collect { case Left(errs) => errs }.flatten
    val rootErrors =
      if validId(data.rootId.trim) then Nil
      else List("Root id must use lowercase letters, digits, and hyphens")
    if parsed.exists(_.isLeft) || rootErrors.nonEmpty then
      Left((rootErrors ++ nodeErrors) match
        case Nil => List("The flow could not be read")
        case errs => errs)
    else
      val nodes = parsed.collect { case Right(node) => node }
      val rootId = NodeId(data.rootId.trim)
      val report = AppealFlowValidation(rootId, nodes)
      if report.errors.nonEmpty then Left(report.errors)
      else Right(AppealFlow(rootId, nodes))

  private def blank(kind: String) = NodeData(
    id = "",
    kind = kind,
    answerer = "user",
    question = "",
    branches = if kind == "choice" then List(BranchData("", "", "")) else Nil,
    text = "",
    close = false,
    unmark = false,
    sleepMonths = none
  )

  private def buildNode(data: NodeData, index: Int): Either[List[String], AppealNode] =
    val label = s"Node ${index + 1}"
    val errors = scala.collection.mutable.ListBuffer.empty[String]
    val id = data.id.trim
    if !validId(id) then errors += s"$label id must use lowercase letters, digits, and hyphens"
    val node = data.kind match
      case "choice" => buildChoice(data, label, id, errors)
      case "action" => buildAction(data, label, id, errors)
      case _ =>
        errors += s"$label type must be choice or action"
        none
    node
      .filter(_ => errors.isEmpty)
      .toRight(errors.toList match
        case Nil => List(s"$label could not be read")
        case errs => errs)

  private def buildChoice(
      data: NodeData,
      label: String,
      id: String,
      errors: scala.collection.mutable.ListBuffer[String]
  ): Option[AppealNode] =
    val answerer = data.answerer match
      case "user" => Answerer.User.some
      case "mod" => Answerer.Mod.some
      case _ =>
        errors += s"$label needs an answerer"
        none
    val question = data.question.trim
    if question.isEmpty then errors += s"$label needs a question"
    if question.length > 4000 then errors += s"$label question is too long"
    val filled = data.branches.filter: branch =>
      branch.id.trim.nonEmpty || branch.answer.trim.nonEmpty || branch.next.trim.nonEmpty
    if filled.isEmpty then errors += s"$label needs an answer"
    val branches = filled.zipWithIndex.flatMap: (branch, index) =>
      buildBranch(s"$label answer ${index + 1}", branch) match
        case Left(errs) =>
          errors ++= errs
          Nil
        case Right(built) => List(built)
    for
      who <- answerer
      answers <- NonEmptyList.fromList(branches).filter(_ => filled.size == branches.size && errors.isEmpty)
    yield ChoiceNode(NodeId(id), who, question, answers)

  private def buildAction(
      data: NodeData,
      label: String,
      id: String,
      errors: scala.collection.mutable.ListBuffer[String]
  ): Option[AppealNode] =
    val text = data.text.trim
    if text.isEmpty then errors += s"$label needs a message"
    if text.length > 5000 then errors += s"$label message is too long"
    data.sleepMonths.foreach: months =>
      if months < 1 || months > 60 then errors += s"$label sleep must be between 1 and 60 months"
    val effects =
      Option.when(data.unmark)(AppealEffect.Unmark).toList :::
        Option.when(data.close)(AppealEffect.Close).toList :::
        data.sleepMonths.toList.map(AppealEffect.Sleep.apply)
    Option.when(errors.isEmpty)(ActionNode(NodeId(id), text, effects))

  private def buildBranch(label: String, data: BranchData): Either[List[String], AnswerBranch] =
    val errors = scala.collection.mutable.ListBuffer.empty[String]
    val id = data.id.trim
    val answer = data.answer.trim
    val next = data.next.trim
    if !validId(id) then errors += s"$label id must use lowercase letters, digits, and hyphens"
    if answer.isEmpty then errors += s"$label needs text"
    if answer.length > 1000 then errors += s"$label text is too long"
    if !validId(next) then errors += s"$label next node must use lowercase letters, digits, and hyphens"
    if errors.nonEmpty then Left(errors.toList)
    else Right(AnswerBranch(AnswerId(id), answer, NodeId(next)))

  private val idChars = "^[a-z0-9]+(-[a-z0-9]+)*$".r
  private def validId(id: String) = id.nonEmpty && id.length <= 64 && idChars.matches(id)

  private def parsePair(raw: String): Option[(Int, Int)] =
    raw.split(':') match
      case Array(a, b) => (a.toIntOption, b.toIntOption).mapN((_, _))
      case _ => none
