package lila.appeal
package ui

import play.api.data.Form
import play.api.data.Field

import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class AppealFlowEditUi(helpers: Helpers):
  import helpers.{ *, given }

  def apply(
      topic: AppealTopic,
      form: Form[AppealFlowForm.Data],
      info: AppealFlowInfo,
      errors: List[String],
      warnings: List[String],
      menu: Frag
  )(using Context) =
    Page(s"${topic.key} appeal flow")
      .css("mod.misc", "bits.form3", "bits.appeal"):
        main(cls := "page-menu")(
          menu,
          div(cls := "page-menu__content box box-pad appeal-flow-edit")(
            boxTop(
              h1(
                s"${topic.key} appeal flow",
                small(
                  " / ",
                  a(href := routes.Appeal.modQueue)("Queue"),
                  " / ",
                  topicLinks(topic)
                )
              )
            ),
            standardFlash,
            info.loadError.map: err =>
              p(cls := "error")(s"Stored flow ignored: $err"),
            p(cls := "appeal-flow-edit__note")(
              info.updatedAt.fold(frag("Not edited yet.")): at =>
                frag(
                  "Last saved ",
                  momentFromNow(at),
                  info.updatedBy.map: by =>
                    frag(" by ", userIdLink(by.some))
                )
            ),
            p(cls := "appeal-flow-edit__note")(
              "Renaming or deleting a node id changes what every open appeal currently on that node sees next. Messages already sent stay as they were."
            ),
            errors.map(err => p(cls := "error")(err)),
            warnings.map(warning => p(cls := "appeal-flow-edit__warning")(warning)),
            postForm(action := routes.Appeal.flowUpdate(topic))(
              form3.globalError(form),
              form3.group(
                form("rootId"),
                "Root node id",
                help = frag("The first node of a new appeal. Must match a node id below.").some
              )(form3.input(_)),
              form("nodes").indexes.map: i =>
                nodeBlock(form(s"nodes[$i]"), i, form("nodes").indexes.size),
              div(cls := "appeal-flow-edit__add")(
                submitButton(cls := "button button-empty", name := "addChoice", value := "1")(
                  "Add choice node"
                ),
                submitButton(cls := "button button-empty", name := "addAction", value := "1")(
                  "Add action node"
                )
              ),
              form3.action(
                form3.submit(
                  "Save",
                  confirm = "This updates the live appeal flow for new steps.".some
                )
              )
            )
          )
        )

  private def topicLinks(topic: AppealTopic) =
    frag:
      AppealTopicApi.usesNewAppealFlow.toList
        .sortBy(_.key)
        .zipWithIndex
        .map: (other, i) =>
          frag(
            (i > 0).option(" · "),
            if other == topic then strong(other.key)
            else a(href := routes.Appeal.flowEdit(other))(other.key)
          )

  private def nodeBlock(node: Field, index: Int, nodeCount: Int)(using Context) =
    val kind = node("kind").value.filter(_.nonEmpty) | "choice"
    val label = node("id").value.filter(_.nonEmpty).fold(s"Node ${index + 1}")(id => s"${index + 1}. $id")
    div(cls := "appeal-flow-edit__node")(
      div(cls := "appeal-flow-edit__node__bar")(
        h2(label),
        span(
          (index > 0).option(
            submitButton(
              cls := "button button-empty",
              name := "moveNode",
              value := s"$index:-1",
              title := "Move up"
            )("Up")
          ),
          (index < nodeCount - 1).option(
            submitButton(
              cls := "button button-empty",
              name := "moveNode",
              value := s"$index:1",
              title := "Move down"
            )("Down")
          ),
          (nodeCount > 1).option(
            submitButton(
              cls := "button button-empty button-red yes-no-confirm",
              name := "removeNode",
              value := index.toString,
              title := "Remove this node?"
            )("Remove")
          )
        )
      ),
      form3.split(
        form3.group(node("id"), "Id", half = true)(form3.input(_)),
        form3.group(node("kind"), "Type", half = true):
          form3.select(_, Seq("choice" -> "Choice", "action" -> "Action"))
      ),
      if kind == "action" then actionFields(node)
      else choiceFields(node, index)
    )

  private def choiceFields(node: Field, index: Int)(using Context) =
    frag(
      form3.group(node("answerer"), "Who answers"):
        form3.select(_, Seq("user" -> "User", "mod" -> "Moderator"))
      ,
      form3.group(node("question"), "Question")(form3.textarea(_)(rows := 4)),
      node("branches").indexes.map: j =>
        branchBlock(node(s"branches[$j]"), index, j),
      submitButton(cls := "button button-empty", name := "addBranch", value := index.toString)("Add answer")
    )

  private def branchBlock(branch: Field, nodeIndex: Int, branchIndex: Int)(using Context) =
    div(cls := "appeal-flow-edit__branch")(
      form3.split(
        form3.group(branch("id"), "Answer id", half = true)(form3.input(_)),
        form3.group(branch("next"), "Next node id", half = true)(form3.input(_))
      ),
      form3.group(branch("answer"), "Answer")(form3.textarea(_)(rows := 2)),
      submitButton(
        cls := "button button-empty button-red",
        name := "removeBranch",
        value := s"$nodeIndex:$branchIndex"
      )("Remove answer")
    )

  private def actionFields(node: Field)(using Context) =
    frag(
      form3.group(node("text"), "Message")(form3.textarea(_)(rows := 8)),
      form3.split(
        form3.checkboxGroup(node("close"), "Close the appeal", half = true),
        form3.checkboxGroup(node("unmark"), "Remove the mark", half = true)
      ),
      form3.group(
        node("sleepMonths"),
        "Sleep for months",
        help = frag("Leave empty to skip the waiting period.").some
      )(form3.input(_, typ = "number"))
    )
