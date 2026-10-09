package lila.appeal

import reactivemongo.api.bson.*
import reactivemongo.api.bson.Macros.Annotations.Key
import lila.db.dsl.*

case class AppealFlowInfo(
    updatedAt: Option[Instant],
    updatedBy: Option[UserId],
    loadError: Option[String]
)

case class StoredAppealFlow(
    @Key("_id") topic: AppealTopic,
    rootId: NodeId,
    nodes: List[AppealNode],
    updatedAt: Option[Instant],
    updatedBy: Option[UserId]
)

final class AppealFlowApi(coll: Coll)(using Executor):

  import BsonHandlers.given

  private val logger = lila.log("appeal")
  @volatile private var meta: Map[AppealTopic, AppealFlowInfo] = Map.empty

  def info(topic: AppealTopic): AppealFlowInfo =
    meta.getOrElse(topic, AppealFlowInfo(none, none, none))

  def warmup(): Unit =
    seedMissing()
      .flatMap(_ => refresh())
      .recover: err =>
        logger.warn(s"Appeal flow warmup failed: ${err.getMessage}")

  def refresh(): Funit =
    coll
      .list[Bdoc](bdoc())
      .map: docs =>
        val updates = scala.collection.mutable.Map.empty[AppealTopic, AppealFlow]
        val infos = scala.collection.mutable.Map.empty[AppealTopic, AppealFlowInfo]
        docs.foreach: doc =>
          doc
            .getAsOpt[AppealTopic]("_id")
            .filter(AppealTopicApi.usesNewAppealFlow.contains)
            .foreach: topic =>
              summon[BSONDocumentHandler[StoredAppealFlow]].readOpt(doc) match
                case None =>
                  infos += topic -> AppealFlowInfo(
                    none,
                    none,
                    s"$topic could not be read and is being ignored".some
                  )
                  logger.warn(s"Appeal flow $topic could not be read")
                case Some(stored) =>
                  val report = AppealFlowValidation(stored.rootId, stored.nodes)
                  if report.errors.nonEmpty then
                    val message = report.errors.mkString(" ")
                    infos += topic -> AppealFlowInfo(stored.updatedAt, stored.updatedBy, message.some)
                    logger.warn(s"Appeal flow $topic ignored: $message")
                  else
                    updates += topic -> AppealFlow(stored.rootId, stored.nodes)
                    infos += topic -> AppealFlowInfo(stored.updatedAt, stored.updatedBy, none)
        AppealFlow.replace(updates.toMap)
        meta = infos.toMap
      .recover: err =>
        logger.warn(s"Appeal flow refresh failed: ${err.getMessage}")

  def save(topic: AppealTopic, flow: AppealFlow, by: UserId): Funit =
    val stored = StoredAppealFlow(topic, flow.rootId, flow.nodes, nowInstant.some, by.some)
    coll.update
      .one(bid(topic), stored, upsert = true)
      .void
      .map: _ =>
        AppealFlow.update(topic, flow)
        meta = meta.updated(topic, AppealFlowInfo(stored.updatedAt, stored.updatedBy, none))

  private def seedMissing(): Funit =
    AppealTopicApi.usesNewAppealFlow.toList.sequentiallyVoid: topic =>
      coll
        .exists(bid(topic))
        .flatMap: exists =>
          if exists then funit
          else
            AppealFlow.defaults
              .get(topic)
              .so: flow =>
                val stored = StoredAppealFlow(topic, flow.rootId, flow.nodes, none, none)
                coll.insert.one(stored).void.recover(lila.db.ignoreDuplicateKey)
