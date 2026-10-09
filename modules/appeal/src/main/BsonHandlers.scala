package lila.appeal

import scala.util.Success
import reactivemongo.api.bson.*

import lila.db.BSON
import lila.db.dsl.{ *, given }

private object BsonHandlers:

  import Appeal.Status
  import AppealMsg.Kind

  given BSONHandler[Status] = stringAnyValHandler(_.toString, t => Status(t) | Status.read)

  given BSONHandler[AppealTopic] =
    stringAnyValHandler(_.key, t => AppealTopic.byKey.getOrElse(t, AppealTopic.legacy))

  given legacyHandler: BSONDocumentHandler[LegacyMessage] = Macros.handler
  given choiceHandler: BSONDocumentHandler[ChoiceEvent] = Macros.handler
  given messageHandler: BSONDocumentHandler[MessageEvent] = Macros.handler
  given actionHandler: BSONDocumentHandler[ActionEvent] = Macros.handler

  given BSONHandler[AppealMsg] = new BSON[AppealMsg]:
    def reads(r: BSON.Reader): AppealMsg =
      r.strO("kind").flatMap(Kind.apply) match
        case None => legacyHandler.readTry(r.doc).get
        case Some(Kind.choice) => choiceHandler.readTry(r.doc).get
        case Some(Kind.message) => messageHandler.readTry(r.doc).get
        case Some(Kind.action) => actionHandler.readTry(r.doc).get
    def writes(w: BSON.Writer, msg: AppealMsg) =
      val doc = msg match
        case m: LegacyMessage => legacyHandler.writeTry(m).get
        case m: MessageEvent => messageHandler.writeTry(m).get
        case m: ChoiceEvent => choiceHandler.writeTry(m).get
        case m: ActionEvent => actionHandler.writeTry(m).get
      msg.kind.fold(doc)(k => doc ++ bdoc("kind" -> k.key))

  given BSONDocumentHandler[AccountsDisclosure] = Macros.handler
  given BSONDocumentHandler[Appeal] = Macros.handler

  given BSONHandler[Answerer] = tryHandler(
    {
      case BSONString("user") => Success(Answerer.User)
      case BSONString("mod") => Success(Answerer.Mod)
    },
    {
      case Answerer.User => BSONString("user")
      case Answerer.Mod => BSONString("mod")
    }
  )

  given BSONHandler[AppealEffect] = tryHandler(
    { case doc: BSONDocument =>
      doc.getAsOpt[String]("t") match
        case Some("close") => Success(AppealEffect.Close)
        case Some("unmark") => Success(AppealEffect.Unmark)
        case Some("sleep") =>
          doc
            .getAsOpt[Int]("months")
            .fold(handlerBadValue("sleep months")): months =>
              Success(AppealEffect.Sleep(months))
        case other => handlerBadValue(s"effect $other")
    },
    {
      case AppealEffect.Close => bdoc("t" -> "close")
      case AppealEffect.Unmark => bdoc("t" -> "unmark")
      case AppealEffect.Sleep(months) => bdoc("t" -> "sleep", "months" -> months)
    }
  )

  given BSONDocumentHandler[AnswerBranch] = new BSON[AnswerBranch]:
    def reads(r: BSON.Reader) =
      AnswerBranch(r.get[AnswerId]("id"), r.str("answer"), r.get[NodeId]("nextNodeId"))
    def writes(w: BSON.Writer, branch: AnswerBranch) =
      bdoc("id" -> branch.id, "answer" -> branch.answer, "nextNodeId" -> branch.nextNodeId)

  given BSONDocumentHandler[AppealNode] = new BSON[AppealNode]:
    def reads(r: BSON.Reader): AppealNode =
      r.str("kind") match
        case "choice" =>
          ChoiceNode(
            r.get[NodeId]("id"),
            r.get[Answerer]("answerer"),
            r.str("question"),
            r.get[List[AnswerBranch]]("branches")
              .toNel
              .getOrElse(sys.error("choice node has no branches"))
          )
        case "action" =>
          ActionNode(r.get[NodeId]("id"), r.str("text"), r.getsD[AppealEffect]("effects"))
        case other => sys.error(s"unknown appeal node $other")
    def writes(w: BSON.Writer, node: AppealNode) = node match
      case cn: ChoiceNode =>
        bdoc(
          "kind" -> "choice",
          "id" -> cn.id,
          "answerer" -> cn.answerer,
          "question" -> cn.question,
          "branches" -> cn.branches.toList
        )
      case an: ActionNode =>
        bdoc("kind" -> "action", "id" -> an.id, "text" -> an.text, "effects" -> an.effects)

  given BSONDocumentHandler[StoredAppealFlow] = Macros.handler
