package framework.generator.spark

import scalaj.http._ //{Http, HttpResponse}
import play.api.libs.json.Json

case class ZepResponse(status: String, message: String, body: String)
case class ZepError(exception: String, message: String, stacktrace: String)
case class ZepStatus(status: String)
// type keyword issue
// case class ZepBody(code: String, type: String, msg: String)
// case class ZepError(status: String, body: ZepBody)

class ZeppelinFactory(host: String = "localhost", port: Int = 8085) {

	implicit val zepResponseFormat = Json.format[ZepResponse]
	implicit val zepStatusFormat = Json.format[ZepStatus]
	// implicit val zepBodyFormat = Json.format[ZepBody]
	implicit val zepErrorFormat = Json.format[ZepError]

	val zep = s"http://$host:$port"
	val zepnote = s"$zep/api/notebook"
	val zepint = (id:String) => s"$zep/api/interpreter/setting/restart/$id"
	val zepdel = (id: String) => s"$zep/api/notebook/$id"
	val zeppara = (id: String) => s"$zep/api/notebook/$id/paragraph"
	val zeprun = (nid: String, pid: String) => s"$zep/api/notebook/run/$nid/$pid"

	def restartInterpreter(id: String = "spark"): Boolean = {
		// curl --request PUT http://localhost:8085/api/interpreter/setting/restart/spark
		val response = Http(zepint(id))
			.option(HttpOptions.connTimeout(10000))
			.option(HttpOptions.readTimeout(50000))
			.postForm
			.method("PUT").asString
		if (response.isError){ print(response); false }
		else true		
	}

	def listNotes: String = {
		val request: HttpRequest = Http(zepnote)
			.option(HttpOptions.connTimeout(10000))
			.option(HttpOptions.readTimeout(50000))
		request.asString.body
	}

	// returns notebook id
	def addNote(name: String): String = {
		val response = Http(zepnote)
			.option(HttpOptions.connTimeout(10000))
			.option(HttpOptions.readTimeout(50000))
			.postData(s"""{"name": $name}""")
			.header("content-type", "application/json").asString
		if (response.isError){
			sys.error("Notebook already exists, delete notebook and try again.")
		}else {
			val parsed = Json.parse(response.body).as[ZepResponse]
			parsed.body
		}
	}

	def deleteNote(id: String): Boolean = {
		val response = Http(zepdel(id))
			.option(HttpOptions.connTimeout(10000))
			.option(HttpOptions.readTimeout(50000))
			.postForm
			.method("DELETE")
			.header("content-type", "application/json").asString
		if (response.isError){ print(response); false }
		else true
	}

	// returns paragraph id
	def writeParagraph(noteid: String, para: String): String = {
		val response = Http(zeppara(noteid))
			.option(HttpOptions.connTimeout(10000))
			.option(HttpOptions.readTimeout(50000))
			.postData(para)
			.header("content-type", "application/json").asString
		if (response.isError){
			val parsed = Json.parse(response.body).as[ZepError]
			sys.error(s"${parsed.exception}:\n${parsed.message}")
		}else{
			val parsed = Json.parse(response.body).as[ZepResponse]
			parsed.body
		}
	}

	def runParaSync(noteid: String, paraid: String): String = {
		val response = Http(zeprun(noteid, paraid))
			.option(HttpOptions.connTimeout(10000))
			.option(HttpOptions.readTimeout(50000))
			.postForm
			.header("content-type", "application/json").asString
		if (response.isError){
			print(response)
			sys.error(s"error parsing notebook")
		}else{
			val parsed = Json.parse(response.body)
			(parsed \ "body" \ "msg" \ 0 \ "data").as[String]
		}
	}


}