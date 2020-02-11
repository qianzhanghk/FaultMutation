package muscala


object  MutationTool {


  def main (args: Array[String] ) {
    val conf = new Configuration("conf.txt").loadMapping().enableSparkMutation()
    val inputdir = "/home/qzhang/Programs/scalafiles"
      //"/MuExamples/scala-csv-master"
    val pathtosrc = "/src/main/scala"   // usually /src/main
    val outputdir = "mutatedFiles"
    val ex = new Extractor()
    ex.run(conf,inputdir,pathtosrc, outputdir)
  }
}
