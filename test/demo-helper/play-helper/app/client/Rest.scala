package clients

import scala.concurrent._
import scalaj.http._
import play.api.libs.json._
import scala.util._

trait CRUD[A,C,U,D] {
  def url:String
  def parseDto(body: String) : D
  def toJson(createArgs: C): JsValue
  def toJsonUpdate(updateArgs: U): JsValue
  def parseA(body: JsValue): A

  def create(params: C):Try[D] = {
    val resp =Http(url)
      .header("content-type","application/json")
      .postData(toJson(params).toString)
      .asString
    if(resp.code == 201)
      Success(parseDto(resp.body))
    else
      Failure(new Exception(s"${resp.code} - ${resp.statusLine}: ${resp.body}"))
  }

  def replace(id:String, params:U):Try[D] = {
    val resp =Http(s"${url}/${id}")
      .header("content-type","application/json")
      .postData(toJsonUpdate(params).toString)
      .method("PUT")
      .asString
    if(resp.code == 201)
      Success(parseDto(resp.body))
    else
      Failure(new Exception(s"${resp.code} - ${resp.statusLine}: ${resp.body}"))
  }

  def get(id:String):Try[A] = {
    val request = Http(s"${url}/${id}")
      .header("content-type","application/json")
      .method("GET")
    val resp = request.asString
    if(resp.code == 200){
      Success(parseA(Json.parse(resp.body)))
    } else
        Failure(
          new Exception(s"Request: ${request.method} ${request.url}\n${request.headers}\n\n ${resp.code} - ${resp.statusLine}: ${resp.body}"))
  }

  def set(id: String, params: U):Try[Unit] = {
    val resp =Http(s"${url}/${id}")
      .header("content-type","application/json")
      .postData(toJsonUpdate(params).toString)
      .asString
    if(resp.code == 201)
      Success(Unit)
    else
      Failure(new Exception(s"${resp.code} - ${resp.statusLine}: ${resp.body}"))
  }

  def del(id:String):Try[Unit] = {
    val request = Http(s"${url}/${id}")
      .header("content-type","application/json")
      .method("DELETE")
    val resp = request.asString
    if(resp.code == 204)
      Success(Unit)
    else
      Failure(
        new Exception(s"Request: ${request.method} ${request.url}\n${request.headers}\n\n ${resp.code} - ${resp.statusLine}: ${resp.body}"))
  }

  def ls(params:String):Try[List[A]] = {
    val request = Http(s"${url}${params}")
      .header("content-type","application/json")
      .method("GET")
    val resp = request.asString
    if(resp.code == 200){
      Success(Json.parse(resp.body).as[List[JsValue]].map(parseA))
    } else
        Failure(
          new Exception(s"Request: ${request.method} ${request.url}\n${request.headers}\n\n ${resp.code} - ${resp.statusLine}: ${resp.body}"))
  }
}

case class CreateRouter(name: String, description: String)
case class UpdateRouter(name:Option[String], description: Option[String])
case class IdDto(id:String)
case class Router(ref:String, name: Option[String], description: Option[String])

class RouterService extends CRUD[Router,CreateRouter,UpdateRouter,IdDto] {
  val url = "http://192.168.1.166:8080/comms-router-web-pure/api/routers"
  def parseDto(body: String): IdDto = new IdDto(body)
  def toJson(params: CreateRouter) = Json.toJson(params)(Json.writes[CreateRouter])
  def toJsonUpdate(params: UpdateRouter) = Json.toJson(params)(Json.writes[UpdateRouter])
  def parseA(body: JsValue) = body.as[Router](Json.reads[Router])


  // def chunks = new Iterator[String]{
  //   private var current = base.asString
  //   private var hasMore = Json.parse(current.body).as[List[JsValue]].length > 0
  //   def hasNext = hasMore
  //   def next = {
  //     require(hasMore)
  //     val last = current.body
  //     hasMore = current.headers.contains("X-Next-Token")
  //     if (hasMore) current = base.param("token", current.headers("X-Next-Token")(0)).asString
  //     last
  //   }
  // }

  // def all = chunks.map (Json.parse) flatMap( _.as[Iterator[Router]](Json.reads[Router]))

}

case class CreateSkill(name: String, description: String)
case class UpdateSkill(name:Option[String], description: Option[String])
case class Skill(ref:String, name: Option[String], description: Option[String])

class SkillService(router: String) extends CRUD[Skill,CreateSkill,UpdateSkill,IdDto] {
  val url = s"http://192.168.1.166:8080/comms-router-web-pure/api/routers/${router}/skills"
  def parseDto(body: String): IdDto = new IdDto(body)
  def toJson(params: CreateSkill): JsValue = Json.toJson(params)(Json.writes[CreateSkill])
  def toJsonUpdate(params: UpdateSkill) = Json.toJson(params)(Json.writes[UpdateSkill])
  def parseA(body: JsValue) = body.as[Skill](Json.reads[Skill])
}

case class CreateQueue(name: String, description: String)
case class UpdateQueue(name:Option[String], description: Option[String])
case class Queue(ref:String, name: Option[String], description: Option[String])

class QueueService(router: String) extends CRUD[Queue,CreateQueue,UpdateQueue,IdDto] {
  val url = s"http://192.168.1.166:8080/comms-router-web-pure/api/routers/${router}/queues"
  def parseDto(body: String): IdDto = new IdDto(body)
  def toJson(params: CreateQueue): JsValue = Json.toJson(params)(Json.writes[CreateQueue])
  def toJsonUpdate(params: UpdateQueue) = Json.toJson(params)(Json.writes[UpdateQueue])
  def parseA(body: JsValue) = body.as[Queue](Json.reads[Queue])
}

case class CreateAgent(name: String, description: String)
case class UpdateAgent(name:Option[String], description: Option[String])
case class Agent(ref:String, name: Option[String], description: Option[String])

class AgentService(router: String) extends CRUD[Agent,CreateAgent,UpdateAgent,IdDto] {
  val url = s"http://192.168.1.166:8080/comms-router-web-pure/api/routers/${router}/agents"
  def parseDto(body: String): IdDto = new IdDto(body)
  def toJson(params: CreateAgent): JsValue = Json.toJson(params)(Json.writes[CreateAgent])
  def toJsonUpdate(params: UpdateAgent) = Json.toJson(params)(Json.writes[UpdateAgent])
  def parseA(body: JsValue) = body.as[Agent](Json.reads[Agent])
}

case class CreateTask(name: String, description: String)
case class UpdateTask(name:Option[String], description: Option[String])
case class Task(ref:String, name: Option[String], description: Option[String])
case class TaskDto(ref:String) //, size: Integer

class TaskService(router: String) extends CRUD[Task,CreateTask,UpdateTask,TaskDto] {
  val url = s"http://192.168.1.166:8080/comms-router-web-pure/api/routers/${router}/tasks"
  def parseDto(body: String) = new TaskDto(body)
  def toJson(params: CreateTask): JsValue = Json.toJson(params)(Json.writes[CreateTask])
  def toJsonUpdate(params: UpdateTask) = Json.toJson(params)(Json.writes[UpdateTask])
  def parseA(body: JsValue) = body.as[Task](Json.reads[Task])
}

case class CreatePlan(name: String, description: String)
case class UpdatePlan(name:Option[String], description: Option[String])
case class Plan(ref:String, name: Option[String], description: Option[String])

class PlanService(router: String) extends CRUD[Plan,CreatePlan,UpdatePlan,IdDto] {
  val url = s"http://192.168.1.166:8080/comms-router-web-pure/api/routers/${router}/plans"
  def parseDto(body: String): IdDto = new IdDto(body)
  def toJson(params: CreatePlan): JsValue = Json.toJson(params)(Json.writes[CreatePlan])
  def toJsonUpdate(params: UpdatePlan) = Json.toJson(params)(Json.writes[UpdatePlan])
  def parseA(body: JsValue) = body.as[Plan](Json.reads[Plan])
}

object Factory{
  def router() = new RouterService()
  def task(r: Router) = new TaskService(r.ref)
  def agent(r: Router) = new AgentService(r.ref)
  def queue(r: Router) = new QueueService(r.ref)
  def plan(r: Router) = new PlanService(r.ref)
  def skill(r: Router) = new SkillService(r.ref)
}

object Maintain {
  val r = new RouterService
  def deleteTasks(routerRef: String){
    val t = new TaskService(routerRef)
  }

  def clearItems(){
    (Maintain.r ls "?per_page=50").map(
      _.filter(_.ref !="router-ivr").map(Factory.task(_)).map(
        ts => ts.ls("?per_page=50").map(_.map(task => {println(task.ref);ts.del(task.ref)}))))

    (Maintain.r ls "?per_page=50").map(
        _.filter(_.ref !="router-ivr").map(Factory.plan(_)).map(
          ts => ts.ls("?per_page=50").map(_.map(task => {println(task.ref);ts.del(task.ref)}))))

    (Maintain.r ls "?per_page=50").map(
        _.filter(_.ref !="router-ivr").map(Factory.agent(_)).map(
          ts => ts.ls("?per_page=50").map(_.map(task => {println(task.ref);ts.del(task.ref)}))))

    (Maintain.r ls "?per_page=50").map(
        _.filter(_.ref !="router-ivr").map(Factory.queue(_)).map(
          ts => ts.ls("?per_page=50").map(_.map(task => {println(task.ref);ts.del(task.ref)}))))

    (Maintain.r ls "?per_page=50").map(
        _.filter(_.ref !="router-ivr").map(Factory.skill(_)).map(
          ts => ts.ls("?per_page=50").map(_.map(task => {println(task.ref);ts.del(task.ref)}))))

    (Maintain.r ls "?per_page=50").map(
        _.filter(_.ref !="router-ivr").map(Maintain.r del _.ref))
  }
}

class Resource(val url: String) {
  val base = Http(url)
  var etag:String=""
  def chunks = new Iterator[String]{
    private var current = base.asString
    private var hasMore = Json.parse(current.body).as[List[JsValue]].length > 0
    def hasNext = hasMore
    def next = {
      require(hasMore)
      val last = current.body
      hasMore = current.headers.contains("X-Next-Token")
      if (hasMore) current = base.param("token", current.headers("X-Next-Token")(0)).asString
      last
    }
  }
  def all = chunks.map (Json.parse) flatMap( _.as [Iterator[JsValue]])
  def lsRaw():String = base.asString.body
  def ls():List[JsValue] = Json.parse(base.asString.body).as[List[JsValue]]
  def get(ref:String): JsValue = {
    val res = Http(List(base.url,ref).mkString("/")).asString
    println(res.headers)
    etag = res.header("ETag").getOrElse("")
    Json.parse(res.body)
  }
  def set(ref:String, params:JsValue)= {
    Http(List(base.url,ref).mkString("/"))
      .header("content-type","application/json")
      .header("IF-MATCH", etag)
      .postData(params.toString).asString
  }
  def del(ref:String): Unit = Http(List(base.url,ref).mkString("/")).method("DELETE").asString
  def create( seed: JsValue): JsValue = Json.parse(
    base.postData(seed.toString)
      .header("content-type","application/json")
      .asString.body)
}

object api {
  var current = new Resource("http://192.168.1.166:8080/comms-router-web/api/routers")
  def cd(ref:String*):Unit = {
    current = new Resource(current.url :: ref.toList mkString("/"))
  }
  def pop(): Unit = current = {
    new Resource(current.base.url.split("/").sliding(2).map(_.head).toList
      .mkString("/"))
  }
  def ls() = current.ls
  def all() = current.all
  def get(ref:String) = current.get(ref)
  def set(ref:String, params:JsValue) = current.set(ref, params)
  def del(ref:String) = current.del(ref)
  def create(seed:JsValue) = current.create(seed)
  def pwd():String = current.base.url
}
