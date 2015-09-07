import java.io.PrintWriter

import scala.io.Source

/**
 * Created by Joshua on 2015/4/13.
 */
object LatentDirichletAllocation {
  //Array[Array[Int]]
  //original paper-keyword bipartite network
  val DTM = Source.fromFile("data\\BipartiteMaxCompartMatrix").getLines.toArray
    .map(_.split("\t").map(_.toInt)).filter(x => x.count(_==1)>=2)
  //number of topics
  val T: Int = 500
  //Dirichlet parameter (document--topic associations)
  val alpha: Double = 50.toDouble/T.toDouble
  //Dirichlet parameter (topic--term associations)
  val beta: Double = 0.1
  //topic assignments for each keyword Z[i][j] - the topic for j word in i doc
  //same word in same doc from the same Topic (which is differ from LDA,but suitable for my case)!
  val Z = DTM
  //M[i][j] - number of words in document i assigned to topic j
  val M = Array.ofDim[Int](DTM.length,T)
  //N[i][j] - number of instances of word i assigned to topic j
  val N = Array.ofDim[Int](DTM(0).length,T)

  def inferLDA(): Unit ={
    //initial
    for(i<-1 to Z.length;j<-1 to Z(0).length;if(Z(i-1)(j-1)!=0)){
      Z(i-1)(j-1) = (Math.random() * T + 1).toInt;
      M(i-1)(Z(i-1)(j-1) - 1) = M(i-1)(Z(i-1)(j-1) - 1) + 1
      N(j-1)(Z(i-1)(j-1) - 1) = N(j-1)(Z(i-1)(j-1) - 1) + 1
    }
    //Gibbs Sampling
    val ITERATIONS = 1000
    def sampleFullConditional(i:Int,j:Int): Int = {
      //do multinomial sampling via cumulative method
      val p: Array[Double] = (1 to T).map(t => (M(i-1)(t-1).toDouble+alpha)/(M.foldLeft(0)((z,x)=>z+x(t-1)).toDouble+T.toDouble*alpha)*
        (N(j-1)(t-1).toDouble+beta)/(N.foldLeft(0)((z,x)=>z+x(t-1)).toDouble+N.length.toDouble*beta)).toArray
      // cumulate multinomial parameters
      for (t<-1 to T) if(t>1) p(t - 1) += p(t - 2)
      // scaled sample because of unnormalised p[]
      val s:Double = Math.random() * p(T - 1)
      p.filter(_ < s).length + 1
    }
    for(itr<-1 to ITERATIONS;i<-1 to Z.length;j<-1 to Z(0).length;if(Z(i-1)(j-1)!=0)){
      M(i-1)(Z(i-1)(j-1) - 1) = M(i-1)(Z(i-1)(j-1) - 1) - 1
      N(j-1)(Z(i-1)(j-1) - 1) = N(j-1)(Z(i-1)(j-1) - 1) - 1
      Z(i-1)(j-1) = sampleFullConditional(i, j);
      M(i-1)(Z(i-1)(j-1) - 1) = M(i-1)(Z(i-1)(j-1) - 1) + 1
      N(j-1)(Z(i-1)(j-1) - 1) = N(j-1)(Z(i-1)(j-1) - 1) + 1
    }
  }
  def ModelAnalysis() = {
    def stepOne()={
      val out = new PrintWriter("data\\LDA-M")
      M.foreach(x => out.println(x.mkString("\t")))
      out.flush()
      out.close()
    }
    println("stepOne Started")
    stepOne()
    println("stepOne Finished")
    def stepTwo()={
      val out = new PrintWriter("data\\LDA-N")
      N.foreach(x => out.println(x.mkString("\t")))
      out.flush()
      out.close()
    }
    println("stepOne Started")
    stepTwo()
    println("stepOne Finished")
  }
  def main(args: Array[String]): Unit = {
    inferLDA()
    ModelAnalysis()
  }
}
