package muscala

import java.io.File
import org.apache.commons.io.FileUtils
import scala.reflect.runtime.universe._
import scala.tools.reflect.{ ToolBox, ToolBoxError }

class Extractor {

    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    import tb._
    import u._

    def modifyOperator(t: Tree, conf: Configuration, op: String, cl: Transformer): Select = t match {
        case b @ Select(t1, t2) =>
            treeCopy.Select(b, t1, newTermName(conf.getMutation(op, "+")))
        case _ => null
    }

    def parseScalaCode(content: String, conf: Configuration, op: String): (Tree, Boolean) = {
        var isMutated = false
        try {
            val tree = tb.parse(content)
            var functiontag = !conf.getSparkConf()
            val newTree = new Transformer {
                override def transform(tree: Tree): Tree = {
                    tree match {
                        case f1 @ Select(id, name) => {
                            name match {
                                case TermNameTag(a) =>
                                    if (conf.matchMutationTarget(a.toString) && functiontag) {
                                        f1 match {
                                            case b @ Select(t1, t2) =>
                                                val newOp = conf.getMutation(a.toString, op)
                                                if (!a.toString.equalsIgnoreCase(newOp)) {
                                                    isMutated = true
                                                }
                                                super.transform(treeCopy.Select(b, t1, newTermName(newOp)))
                                            case _ => null
                                        }
                                    } else {
                                        val newOp = conf.getSparkMutation(a.toString)
                                        if (!a.toString.equalsIgnoreCase(newOp))
                                            isMutated = true
                                        super.transform(treeCopy.Select(f1, id, newTermName(newOp)))
                                    }
                                case _ =>
                                    super.transform(tree)
                            }
                        }
                        case f @ Function(l, tr) =>
                            functiontag = true
                            val temp = super.transform(f)
                            functiontag = !conf.getSparkConf()
                            temp
                        case t =>
                            super.transform(t)
                    }
                }
            }.transform(tree)
            // if(isMutated)
            //     println(showRaw(newTree))
            (newTree, isMutated)
        } catch {
            case ex: Exception =>
                ex.printStackTrace()
                null
        }
    }

    def saveToFile(filepath: String, path: File, code: Tree) = {
        val pack = packageMap.getOrElse(path.getAbsolutePath, "")
        var c = showCode(code).trim()

        if (c(0) == '{' && c(c.length - 1) == '}') {
            c = c.substring(1)
            c = c.substring(0, c.length - 1).trim
        }
        if (c.endsWith("()")) {
            c = c.substring(0, c.length - 2)
        }
        val writer = new java.io.PrintWriter(filepath)
        try
            writer.write(pack + "\n" + c)
        finally writer.close()
    }

    def mutate(fileName: String, conf: Configuration): List[Tree] = {
        val source = scala.io.Source.fromFile(fileName)
        val lines = try {
            var str = ""
            for (l <- source.getLines()) {
                if (l.startsWith("package")) {
                    packageMap += (fileName -> l)
                } else
                    str += l + "\n"
            }
            str
        } finally {
            source.close()
        }

        var newTreeList: List[Tree] = List()
        for (op <- conf.mutationMapping.keys) {
            val temp = parseScalaCode(lines, conf, op)
            if (temp._2) {
                newTreeList ::= temp._1
            }
        }

        newTreeList
    }

    def getRecursiveListOfFiles(dir: File): Array[File] = {
        val these = dir.listFiles
        these.filter(p => p.getName.contains(".scala")) ++ these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
    }

    var packageMap: Map[String, String] = Map[String, String]()

    def run(conf: Configuration, inputdir: String, pathToSrc: String, outputDir: String): Unit = {
        val start = java.lang.System.currentTimeMillis()
        println(conf.targetOp)
        println(conf.mutationMapping)
        val targetFiles = inputdir + pathToSrc
        val dir = new File(outputDir)
        if (!dir.exists()) {
            dir.mkdir()
        }

        for (scalafile <- getRecursiveListOfFiles(new File(targetFiles))) {
            val subDirIndex = scalafile.toString.indexOf(pathToSrc) + pathToSrc.length
            val filename = scalafile.getName
            try {
                //println(s"""Starting Mutation on  $filename  """)
                var count = 0
                val mutatedList = this.mutate(scalafile.getAbsolutePath, conf)
                for (mutated <- mutatedList) {
                    val mutantDir = outputDir + "/mutant_" + filename.substring(0, filename.length - 6) + "_" + count.toString()
                    FileUtils.copyDirectory(new File(inputdir), new File(mutantDir))
                    val dstFilePath = mutantDir+pathToSrc+scalafile.toString.substring(subDirIndex)
                    saveToFile(dstFilePath, scalafile, mutated)
                    count += 1
                }
                println(s"""Mutation passed on  $filename  """)
            } catch {
                case e: Exception => {
                    e.printStackTrace()
                    println(s"""Mutation failed on  $filename . Skipping.... """)
                }
                case _ => println(s"""Mutation failed on  $filename . Skipping.... """)

            }
        }

        val diff = java.lang.System.currentTimeMillis() - start
        println(diff)
    }

}
