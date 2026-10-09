package lila.appeal

import reactivemongo.api.bson.*
import BsonHandlers.given

class AppealFlowTest extends munit.FunSuite:

  test("built-in flows are valid"):
    AppealTopicApi.usesNewAppealFlow.foreach: topic =>
      val flow = AppealFlow.defaults(topic)
      val report = AppealFlowValidation(flow.rootId, flow.nodes)
      assertEquals(report.errors, Nil, clue = topic)
      assertEquals(report.warnings, Nil, clue = topic)

  test("form roundtrip keeps the built-in flows"):
    AppealTopicApi.usesNewAppealFlow.foreach: topic =>
      val flow = AppealFlow.defaults(topic)
      val source = AppealFlowForm.from(flow)
      val built = AppealFlowForm.build(source).toOption
      assertEquals(
        built.map(parsed => parsed.rootId :: parsed.nodes.map(_.id)),
        (flow.rootId :: flow.nodes.map(_.id)).some,
        clue = topic
      )
      val bound = AppealFlowForm.form.bind(AppealFlowForm.form.fill(source).data)
      assertEquals(bound.errors.map(_.toString), Nil, clue = topic)
      assertEquals(bound.value.flatMap(data => AppealFlowForm.build(data).toOption), built, clue = topic)

  test("bson roundtrip"):
    val flow = AppealFlow.defaults(AppealTopic.cheat)
    val stored = StoredAppealFlow(AppealTopic.cheat, flow.rootId, flow.nodes, none, none)
    val handler = summon[BSONDocumentHandler[StoredAppealFlow]]
    val back = handler.readTry(handler.writeTry(stored).get).get
    assertEquals(back, stored)

  test("cycle is allowed"):
    val report = AppealFlowValidation(
      NodeId("a"),
      List(choice("a", "yes" -> "b"), choice("b", "back" -> "a"))
    )
    assertEquals(report.errors, Nil)
    assertEquals(report.warnings, Nil)

  test("dangling next node"):
    val report = AppealFlowValidation(NodeId("a"), List(choice("a", "yes" -> "missing")))
    assert(report.errors.exists(_.contains("missing")))

  test("unreachable node is a warning"):
    val report = AppealFlowValidation(
      NodeId("a"),
      List(choice("a", "yes" -> "b"), action("b"), action("c"))
    )
    assertEquals(report.errors, Nil)
    assert(report.warnings.exists(_.contains("c")))

  test("duplicate node id"):
    val report = AppealFlowValidation(NodeId("a"), List(action("a"), action("a")))
    assert(report.errors.exists(_.contains("Duplicate")))

  test("duplicate answer id"):
    val report = AppealFlowValidation(
      NodeId("a"),
      List(choice("a", "yes" -> "b", "yes" -> "b"), action("b"))
    )
    assert(report.errors.exists(_.contains("duplicate answer")))

  test("missing checkbox effects bind as absent"):
    val bound = AppealFlowForm.form.bind:
      Map(
        "rootId" -> "done",
        "nodes[0].id" -> "done",
        "nodes[0].kind" -> "action",
        "nodes[0].text" -> "Done"
      )
    val flow = bound.value.flatMap(data => AppealFlowForm.build(data).toOption).get
    assertEquals(flow.nodes, List(ActionNode(NodeId("done"), "Done")))

  test("blank answer row does not satisfy a choice node"):
    val data = AppealFlowForm.Data(
      rootId = "ask",
      nodes = List(
        AppealFlowForm.NodeData(
          id = "ask",
          kind = "choice",
          answerer = "user",
          question = "Q",
          branches = List(AppealFlowForm.BranchData("", "", "")),
          text = "",
          close = false,
          unmark = false,
          sleepMonths = none
        )
      )
    )
    assert(AppealFlowForm.build(data).isLeft)

  test("structural edits keep the other nodes"):
    val data = AppealFlowForm.from(AppealFlow.defaults(AppealTopic.boost))
    val added = AppealFlowForm.applyEdit(data, AppealFlowForm.Edit.AddAction)
    assertEquals(added.nodes.size, data.nodes.size + 1)
    val removed = AppealFlowForm.applyEdit(added, AppealFlowForm.Edit.RemoveNode(added.nodes.size - 1))
    assertEquals(removed.nodes.map(_.id), data.nodes.map(_.id))
    val only = data.copy(nodes = data.nodes.take(1))
    assertEquals(AppealFlowForm.applyEdit(only, AppealFlowForm.Edit.RemoveNode(0)).nodes.size, 1)

  private def choice(id: String, branches: (String, String)*): ChoiceNode =
    ChoiceNode(
      NodeId(id),
      Answerer.User,
      "Question",
      NonEmptyList.fromListUnsafe:
        branches.toList.map: (answerId, next) =>
          AnswerBranch(AnswerId(answerId), answerId, NodeId(next))
    )

  private def action(id: String): ActionNode =
    ActionNode(NodeId(id), "Done")
