package lila.appeal

case class AppealFlowReport(errors: List[String], warnings: List[String]):
  def valid = errors.isEmpty

object AppealFlowValidation:

  def apply(rootId: NodeId, nodes: List[AppealNode]): AppealFlowReport =
    val errors = scala.collection.mutable.ListBuffer.empty[String]
    val idCounts = nodes.groupMapReduce(_.id)(_ => 1)(_ + _)
    idCounts.foreach: (id, n) =>
      if n > 1 then errors += s"Duplicate node id ${id.value}"
    nodes.foreach:
      case cn: ChoiceNode =>
        val answerCounts = cn.branches.toList.groupMapReduce(_.id)(_ => 1)(_ + _)
        answerCounts.foreach: (id, n) =>
          if n > 1 then errors += s"Node ${cn.id.value} has duplicate answer id ${id.value}"
        val known = idCounts.keySet
        cn.branches.toList.foreach: branch =>
          if !known.contains(branch.nextNodeId) then
            errors += s"Node ${cn.id.value} answer ${branch.id.value} points at unknown node ${branch.nextNodeId.value}"
      case _: ActionNode => ()
    if !idCounts.contains(rootId) then errors += s"Root id ${rootId.value} is not a node"
    val warnings =
      if errors.nonEmpty || !idCounts.contains(rootId) then Nil
      else
        val known = nodes.mapBy(_.id)
        (known.keySet -- reachable(rootId, known)).toList
          .sortBy(_.value)
          .map(id => s"Node ${id.value} is not reachable from the root")
    AppealFlowReport(errors.toList, warnings)

  private def reachable(rootId: NodeId, byId: Map[NodeId, AppealNode]): Set[NodeId] =
    val seen = scala.collection.mutable.Set.empty[NodeId]
    val queue = scala.collection.mutable.Queue(rootId)
    while queue.nonEmpty do
      val id = queue.dequeue()
      if seen.add(id) then
        byId
          .get(id)
          .foreach:
            case cn: ChoiceNode => cn.branches.toList.foreach(branch => queue.enqueue(branch.nextNodeId))
            case _: ActionNode => ()
    seen.toSet
