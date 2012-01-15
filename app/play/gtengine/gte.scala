package play.gtengine

import java.lang.String
import play.template2._
import compile.GTPreCompiler.{GTFragmentCode, SourceContext}
import exceptions.{GTRuntimeException, GTCompilationExceptionWithSourceInfo, GTRuntimeExceptionWithSourceInfo, GTTemplateNotFoundWithSourceInfo}
import play.template2.compile._
import compile.GTCompiler
import play.api.templates.Html
import play.data.Form
import play.api.i18n.Messages
import play.api.cache.Cache
import java.util.regex.{Pattern, Matcher}
import play.api.PlayException
import java.io._
import java.net.URL
import org.apache.commons.io.IOUtils


abstract class GTJavaBase2xImpl(groovyClass: Class[_ <: GTGroovyBase], templateLocation: GTTemplateLocation) extends GTJavaBase(groovyClass, templateLocation) {

  var form: Form[_ <: AnyRef] = null

  def getRawDataClass = null

  def convertRawDataToString(p1: AnyRef) = throw new Exception("Not impl yet")

  def escapeHTML(html: String) = org.apache.commons.lang.StringEscapeUtils.escapeHtml(html)

  def escapeXML(xml: String) = org.apache.commons.lang.StringEscapeUtils.escapeXml(xml)

  def escapeCsv(csv: String) = org.apache.commons.lang.StringEscapeUtils.escapeCsv(csv)

  def validationHasErrors(): Boolean = {
    if (form == null) {
      return false
    } else {
      return form.hasErrors
    }
  }

  def validationHasError(name: String): Boolean = {
    if (form == null) {
      return false
    } else {
      val errors = form.errors().get(name);
      if (errors == null || errors.size() == 0) {
        return false;
      }
      return true;
    }
  }

  def messagesGet(key: Any, args: Object*): String = Messages.apply(key.toString, args)

  def cacheGet(key: String) = {
    import play.api.Play.current
    Cache.get(key)
  }

  def cacheSet(key: String, data: AnyRef, duration: String) {
    import play.api.Play.current
    if (duration == null) {
      Cache.set(key.toString, data)
    } else {
      throw new Exception("Cache.set not implemented yet with duration string")
      //Cache.set(key, data, expiration)
    }
  }
}

class GTGroovyBase2xImpl extends GTGroovyBase {
  override def _resolveClass(clazzName: String): Class[_ <: Any] = {
    getClass.getClassLoader.loadClass(clazzName)
  }
}

class GTPreCompiler2xImpl(templateRepo: GTTemplateRepo) extends GTPreCompiler(templateRepo) {

  this.customFastTagResolver = GTFastTagResolver2xImpl

  override def getJavaBaseClass = classOf[GTJavaBase2xImpl]

  override def getGroovyBaseClass = classOf[GTGroovyBase2xImpl]

  // must modify all use of @{} in tag args
  //override def checkAndPatchActionStringsInTagArguments(tagArgs : String) : String = {
  // Not implementing support for @ in args..

  val staticFileP = Pattern.compile("^'(.*)'$")

  override def generateRegularActionPrinter(absolute: Boolean, _action: String, sc: SourceContext, lineNo: Int): GTFragmentCode = {

    var code: String = null;
    var action = _action
    val m: Matcher = staticFileP.matcher(action.trim());
    if (m.find()) {
      // This is an action/link to a static file.
      action = m.group(1); // without ''
      // TODO: needs absolut support
      code = " out.append(controllers.routes.Assets.at(\"" + action + "\").url());\n";
    } else {
      if (!action.endsWith(")")) {
        action = action + "()";
      }

      // generate groovy code
      sc.nextMethodIndex = sc.nextMethodIndex + 1
      val nextMethodIndex: Int = sc.nextMethodIndex
      val groovyMethodName: String = "action_resolver_" + nextMethodIndex;

      sc.gprintln(" String " + groovyMethodName + "() {", lineNo);
      if (absolute) {
        // TODO: needs absolute support
        sc.gprintln(" return _('controllers.routes')." + action + ".url();");
      } else {
        sc.gprintln(" return _('controllers.routes')." + action + ".url();");
      }
      sc.gprintln(" }");

      // generate java code that prints it
      code = " out.append(g." + groovyMethodName + "());";
    }

    return new GTFragmentCode(lineNo, code);
  }

}

class PreCompilerFactory extends GTPreCompilerFactory {
  def createCompiler(templateRepo: GTTemplateRepo) = new GTPreCompiler2xImpl(templateRepo)
}

class GTTypeResolver2xImpl extends GTTypeResolver {

  override def getTypeBytes(clazzName: String): Array[Byte] = {
    val name = clazzName.replace(".", "/") + ".class";
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(name);
    if (is == null) {
      return null;
    }
    try {
      val os = new ByteArrayOutputStream();
      val buffer = new Array[Byte](8192);
      var count = 1;
      while (count > 0) {
        count = is.read(buffer, 0, buffer.length)
        if (count > 0) {
          os.write(buffer, 0, count);
        }
      }
      return os.toByteArray();
    } finally {
      is.close();
    }
  }
}

class GTFileResolver2xImpl(folder: File) extends GTFileResolver.Resolver {
  def getTemplateLocationReal(queryPath: String): GTTemplateLocationReal = {
    val file = new File(folder, queryPath);
    if (file.exists()) {
      return new GTTemplateLocationReal(queryPath, file.toURI.toURL)
    } else {
      return null;
    }
  }

  def getTemplateLocationFromRelativePath(relativePath: String): GTTemplateLocationReal = {
    val file = new File(folder, relativePath);
    if (file.exists()) {
      return new GTTemplateLocationReal(relativePath, file.toURI.toURL)
    } else {
      return null;
    }
  }
}


class GTETemplate(gtJavaBase: GTJavaBase) {

  def render(params: java.util.Map[String, AnyRef]): Html = {
    import scala.collection.JavaConversions._
    _render(params.toMap)
  }

  def render(params: Map[String, AnyRef]): Html = {
    _render(params)
  }

  private def _render(params: Map[String, AnyRef]): Html = {
    import scala.collection.JavaConversions._

    gteHelper.exceptionTranslator( { () =>
      gtJavaBase.renderTemplate(params)
    })

    new Html(gtJavaBase.getAsString)
  }
}

class GTJavaExtensionMethodResolver2xImpl extends GTJavaExtensionMethodResolver {
  def findClassWithMethod(methodName: String) = null
}

object gte {

  GTCompiler.srcDestFolder = new File("gt-generated-src")

  val viewFolder = "app/gtviews/"
  val parentClassLoader: ClassLoader = getClass.getClassLoader

  GTJavaCompileToClass.typeResolver = new GTTypeResolver2xImpl()
  GTGroovyPimpTransformer.gtJavaExtensionMethodResolver = new GTJavaExtensionMethodResolver2xImpl

  GTFileResolver.impl = new GTFileResolver2xImpl(new File(viewFolder));

  val folderToDumpClassesIn = new File("tmp/gttemplates");
  val repo = new GTTemplateRepo(parentClassLoader, true, new PreCompilerFactory, false, folderToDumpClassesIn)

  def template(path: String): GTETemplate = {
    gteHelper.exceptionTranslator({ () =>
      val gtJavaBase: GTJavaBase = repo.getTemplateInstance( GTFileResolver.impl.getTemplateLocationReal(path))
      new GTETemplate(gtJavaBase)
    })
  }

}

object GTFastTagResolver2xImpl extends GTFastTagResolver {

  val fastTagResolvers = getFastTagResolvers


  def resolveFastTag(tagName: String) : String = {
    for (i <- 0 until fastTagResolvers.size ) {
      val resolver = fastTagResolvers(i)
      val fastTag = resolver.resolveFastTag(tagName)
      if ( fastTag != null) {
        return fastTag
      }
    }

    return null
  }

  private def getFastTagResolvers : List[GTFastTagResolver] = {
    val GT_FASTTAGS_FILENAME = "gt-fasttags.txt"
    import scala.collection.JavaConversions._
    this.getClass.getClassLoader.getResources(GT_FASTTAGS_FILENAME).toList.map( { f : URL =>
      val lines : Array[String] = IOUtils.toString( f.openStream(), "utf-8").split("\\r?\\n")
      lines.map({ clazzName : String =>
        if ( clazzName.trim().startsWith("#") ) {
          // Skip it - comment
          List()
        } else {
          List(this.getClass.getClassLoader.loadClass(clazzName.trim()).asInstanceOf[Class[GTFastTagResolver]].newInstance())
        }
      }).flatten
    } ).flatten
  }

}


object gteHelper {
  
  def exceptionTranslator[T](code: () => T) : T = {
      try {
  
        code()
  
      } catch {
        case e: GTTemplateNotFoundWithSourceInfo => {
          import scalax.io._
          throw new PlayException(
            "Template not found",
            "[%s: %s]".format("Template not found", e.queryPath),
            Some(e)) with PlayException.ExceptionSource {
            def line = Some(e.lineNo)
  
            def position = None
  
            def input = Some(Resource.fromInputStream(new ByteArrayInputStream(e.templateLocation.readSource().getBytes("utf-8"))))
  
            def sourceName = Some(e.templateLocation.relativePath)
          }
        }
        case e: GTRuntimeExceptionWithSourceInfo => {
          import scalax.io._
          throw new PlayException(
            "Template Runtime Exception",
            "[%s: %s]".format(e.getCause.getClass.getSimpleName, e.getCause.getMessage),
            Some(e.getCause)) with PlayException.ExceptionSource {
            def line = Some(e.lineNo)
  
            def position = None
  
            def input = Some(Resource.fromInputStream(new ByteArrayInputStream(e.templateLocation.readSource().getBytes("utf-8"))))
  
            def sourceName = Some(e.templateLocation.relativePath)
          }
        }
        case e: GTCompilationExceptionWithSourceInfo => {
          import scalax.io._
          throw new PlayException(
            "Template Compilation Error",
            "[%s: %s]".format(e.getClass.getSimpleName, e.getMessage),
            Some(e)) with PlayException.ExceptionSource {
            def line = Some(e.oneBasedLineNo)
  
            def position = None
  
            def input = Some(Resource.fromInputStream(new ByteArrayInputStream(e.templateLocation.readSource().getBytes("utf-8"))))
  
            def sourceName = Some(e.templateLocation.relativePath)
          }
        }
        case e: GTRuntimeException => {
          val cause: Throwable = if (e.getCause == null) {
            e
          } else {
            e.getCause()
          }
          import scalax.io._
          throw new PlayException(
            "Template Runtime Exception",
            "[%s: %s]".format(cause.getClass.getSimpleName, cause.getMessage),
            Some(e.getCause)) with PlayException.ExceptionSource {
            def line = None
  
            def position = None
  
            def input = None
  
            def sourceName = None
          }
        }
      }
  }

}