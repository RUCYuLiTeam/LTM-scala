import scala.io.Source
import java.io._

/**
 * Created by Joshua on 2015/4/1.
 */
object LinkTopicModel {
  def bipartiteMaximumSpanningTreeProjecting(bipartite: Array[Array[Int]]): Array[Array[Int]] = {
    //克鲁斯卡尔算法
    def extractMaximumSpanningTree(paper: Array[Int]): Array[Array[Int]] = {
      val edgeList = (for (i <- paper.indexOf(1) to paper.lastIndexOf(1); j = paper.indexOf(1, i + 1);
                           if (paper(i) == 1 & j > 0)) yield (i + 1, j + 1, bipartite.count(x => (x(i) != 0 & x(j) != 0))))
        .sortBy(_._3).reverseMap(x => Array(x._1, x._2)).toArray
      val maxKey = (for (e <- edgeList;k <- e) yield k).distinct.size
      val edgeListMST = Array(edgeList(0)).toBuffer
      var edge = 1
      val keyListMST = (for (e <- edgeListMST;k <- e) yield k).distinct.toBuffer
      while(keyListMST.size < maxKey){
        if(!edgeList(edge).diff(keyListMST).isEmpty){
          keyListMST.appendAll(edgeList(edge).diff(keyListMST))
          edgeListMST.append(edgeList(edge))
        }
        edge = edge + 1
      }
      edgeListMST.toArray
    }
    (for(paper <- bipartite;edge <- extractMaximumSpanningTree(paper)) yield (edge(0),edge(1))).distinct
      .zipWithIndex.map(x => Array(x._1._1, x._1._2, x._2 + 1))
  }
  // similarity of each edge
  def calculateCoNeighborSimilarityOfAllEdges(edges: Array[Array[Int]]): Map[Set[Int], Double] = {
    def calculateStructureSimilarity(i: Int,j: Int):Double ={
      val aSet = edges.foldLeft(Set(i))((z,e) => if (e(0)==i|e(1)==i) (z ++ Set(e(0),e(1))) else z)
      val bSet = edges.foldLeft(Set(j))((z,e) => if (e(0)==j|e(1)==j) (z ++ Set(e(0),e(1))) else z)
      (aSet & bSet).size.toDouble/(aSet | bSet).size.toDouble
    }
    def calculateSimilarity(edge_a: Set[Int], edge_b: Set[Int]):Double={
      if((edge_a & edge_b).isEmpty) return  0
      if(edge_a == edge_b) return 1
      //P(ij|k)
      val i = (edge_a  diff (edge_a & edge_b)).toArray.apply(0)
      val j = (edge_b  diff (edge_a & edge_b)).toArray.apply(0)
      calculateStructureSimilarity(i,j)
    }
    (for (edge_a <- edges;edge_b <- edges.filter(_(2)>edge_a(2));
          sim=calculateSimilarity(edge_a.take(2).toSet,edge_b.take(2).toSet);if(sim!=0))
    yield (Set(edge_a(2),edge_b(2)) -> sim)).toMap
  }
  def calculateCoNeighborBasedSimilarityOfAllEdges(bipartite: Array[Array[Int]], edges: Array[Array[Int]]): Map[Set[Int], Double] = {
    def calculateStructureSimilarity(i: Int,j: Int,k: Int):Double ={
      val aSet = edges.foldLeft(Set(i))((z,e) => if (e(0)==i|e(1)==i) (z ++ Set(e(0),e(1))) else z)
      val bSet = edges.foldLeft(Set(j))((z,e) => if (e(0)==j|e(1)==j) (z ++ Set(e(0),e(1))) else z)
      val cSet = edges.foldLeft(Set(k))((z,e) => if (e(0)==k|e(1)==k) (z ++ Set(e(0),e(1))) else z)
      (aSet & bSet & cSet).size.toDouble/(aSet | bSet | cSet).size.toDouble
    }
    def calculateSimilarity(edge_a: Set[Int], edge_b: Set[Int]):Double={
      if((edge_a & edge_b).isEmpty) return  0
      if(edge_a == edge_b) return 1
      //P(ij|k)
      val i = (edge_a  diff (edge_a & edge_b)).toArray.apply(0)-1
      val j = (edge_b  diff (edge_a & edge_b)).toArray.apply(0)-1
      val k = (edge_a & edge_b).toArray.apply(0)-1
      val alpha = calculateStructureSimilarity(i+1,j+1,k+1)
      val N_1 = bipartite.count(x => (x(i)!=0&x(j)!=0&x(k)!=0))
      val N_2 = bipartite.count(x => (x(i)!=0|x(j)!=0|x(k)!=0))
      ((1-alpha)*N_1+alpha*N_2)/bipartite.count(_(k)!=0)
    }
    (for (edge_a <- edges;edge_b <- edges.filter(_(2)>edge_a(2));
          sim=calculateSimilarity(edge_a.take(2).toSet,edge_b.take(2).toSet);if(sim!=0))
    yield (Set(edge_a(2),edge_b(2)) -> sim)).toMap
  }
  def calculateInsideLinkBasedSimilarityOfAllEdges(bipartite: Array[Array[Int]], edges: Array[Array[Int]]): Map[Set[Int], Double] = {
    val alpha = 0.55
    val beta = 0.15
    def calculateSimilarity(edge_a: Set[Int], edge_b: Set[Int]):Double={
      if((edge_a & edge_b).isEmpty) return  0
      if(edge_a == edge_b) return 1
      //P(ij|k)
      val i = (edge_a  diff (edge_a & edge_b)).toArray.apply(0)-1
      val j = (edge_b  diff (edge_a & edge_b)).toArray.apply(0)-1
      val k = (edge_a & edge_b).toArray.apply(0)-1
      val N_1 = bipartite.count(x => (x(i)!=0&x(j)!=0&x(k)!=0))
      val N_2 = Array(bipartite.count(x => (x(i)!=0&x(j)!=0)),
        bipartite.count(x => (x(i)!=0&x(k)!=0)),
        bipartite.count(x => (x(j)!=0&x(k)!=0))).min
      val N_3 = Array(bipartite.count(x => (x(i)!=0)),
        bipartite.count(x => (x(k)!=0)),
        bipartite.count(x => (x(j)!=0))).min
      ((1-alpha)*N_1+(alpha-beta)*N_2+beta*N_3)/bipartite.count(_(k)!=0)
    }
    (for (edge_a <- edges;edge_b <- edges.filter(_(2)>edge_a(2));
          sim=calculateSimilarity(edge_a.take(2).toSet,edge_b.take(2).toSet);if(sim!=0))
    yield (Set(edge_a(2),edge_b(2)) -> sim)).toMap
  }
  //Link Community Tree with file output
  def generateLinkCommunityTree(coTerm: Array[Array[Int]], sortedEdgeSet: List[Set[Set[Int]]], file: String) = {
    val out = new PrintWriter(file)
    val layer = coTerm.map(_.last).toBuffer
    var rankLevel = 0
    while (layer.distinct.size != 1 & rankLevel < sortedEdgeSet.size) {
      //write file of Last Layer
      println(rankLevel-1, "writing!")
      out.println(layer.mkString("\t"))
      out.flush()
      println(rankLevel, sortedEdgeSet(rankLevel).size,sortedEdgeSet.size)
      //next new layer generation
      for (es <- sortedEdgeSet(rankLevel);
           old_a = layer(es.toList(0)-1);
           old_b = layer(es.toList(1)-1);
           m = old_a min old_b;
           n <- 0 to layer.size - 1;
           if (layer(n) !=m &( layer(n) == old_a | layer(n) ==old_b))) {
        layer.update(n, m)
      }
      rankLevel = rankLevel + 1
    }
    out.close()
  }
  def ModelGeneration()={
    //Array[Array[Int]]
    //original paper-keyword bipartite network
    val bipartite = Source.fromFile("data\\BipartiteMaxCompartMatrix").getLines.toArray
      .map(_.split("\t").map(_.toInt)).filter(x => x.count(_==1)>=2)
    def stepOne() = {
      //Maximum Spanning Tree co-term of each paper
      val cotermMST = bipartiteMaximumSpanningTreeProjecting(bipartite)
      //write file
      val out = new PrintWriter("data\\cotermMST")
      cotermMST.foreach(x => out.println(x.mkString("\t")))
      out.flush()
      out.close()
    }
    println("stepOne Started")
    //stepOne()
    println("stepOne Finished")
    //Array[Array[Int]]
    val cotermMST = Source.fromFile("data\\cotermMST").getLines.toArray
      .map(_.split("\t").map(_.toInt))
    def stepTwo() = {
      //similarity
      val edgeSimilarity = calculateCoNeighborBasedSimilarityOfAllEdges(bipartite,cotermMST)
      //write file
      val out = new PrintWriter("data\\similarityMST")
      edgeSimilarity.foreach(x => out.println(x._1.mkString("\t").concat("\t").concat(x._2.toString)))
      out.flush()
      out.close()
    }
    println("stepTwo Started")
    stepTwo()
    println("stepTwo Finished")
    val edgeSimilarity = Source.fromFile("data\\similarityMST").getLines.toArray
      .map(_.split("\t")).map(x => (Set(x(0).toInt,x(1).toInt) -> x(2).toDouble)).toMap
    def stepThree() = {
      //List(Set(Set(i,j)))
      val sortedEdgeSet =edgeSimilarity.groupBy(_._2).toList.sortWith((a,b) => a._1 > b._1).map(_._2.keySet)
      generateLinkCommunityTree(cotermMST,sortedEdgeSet,"F:\\Github\\LinkTopicModel\\data\\LinkTopicTree")
    }
    println("stepThree Started")
    stepThree()
    println("stepThree Finished")
  }
  def averagePartitionDensity(layer: Array[Int],coterm: Array[Array[Int]],bipartite:Array[Array[Int]]): Double = {
    def calPartitionDensity(cluster: Int): (Double,Double) = {
      val c = coterm.filter(x => layer(x(2)-1)==cluster)
      val m = c.length.toDouble
      val n = (c.foldLeft(Set(0))((z,x) => z++Set(x(0),x(1))) - 0).size.toDouble
      if(n==2) (0,m) else ((m - (n - 1))/(n * (n - 1) / 2 - (n - 1)),m)
    }
    (for(cluster <- layer.distinct) yield calPartitionDensity(cluster))
      .map(x => x._1*x._2).sum/coterm.length
  }
  def ModelThetaOmegaAnalysis(n:Int = 5888) = {
    val bipartite = Source.fromFile("data\\BipartiteMaxCompartMatrix").getLines.toArray
      .map(_.split("\t").map(_.toInt)).filter(x => x.count(_==1)>=2)
    val cotermMST = Source.fromFile("data\\cotermMST").getLines.toArray
      .map(_.split("\t").map(_.toInt))
    val coterm = Source.fromFile("data\\coterm").getLines.toArray
      .map(_.split("\t").map(_.toInt))
    def stepOneLower()={
      val out = new PrintWriter("data\\PartitionDensity")
      for (line <- Source.fromFile("data\\LinkTopicTree").getLines){
        val l = line.split("\t").map(_.toInt)
        out.println(averagePartitionDensity(l,cotermMST,bipartite).toString
          .concat("\t").concat(l.distinct.length.toString))
        out.flush()
      }
      out.close()
    }
    println("stepOne Lower Started")
    //stepOneLower()
    println("stepOne Lower Finished")
    def stepOneUpper()={
      val out = new PrintWriter("data\\CombinationInfluence")
      var topicLast = Map(0 -> 0)
      for (model <- Source.fromFile("data\\LinkTopicTree").getLines){
        val topicNow = model.split("\t").map(_.toInt).foldLeft(Map(0->0))((s,x) => if(s.get(x).isEmpty) s+(x -> 1) else s.updated(x, (s.get(x).get+1))).-(0)
        val ATCI = if(!topicLast.get(0).isEmpty){
          0.0
        }else{
          topicNow.map(x => x._2 - topicLast.get(x._1).get).sum.toFloat/topicNow.size.toFloat
        }
        topicLast = topicNow
        out.println(ATCI)
        out.flush()
      }
      out.close()
    }
    println("stepOne Upper Started")
    //stepOneUpper()
    println("stepOne Upper Finished")
    //start from 0!get the nth layer of tree as the model
    val model = Source.fromFile("data\\LinkTopicTree").getLines.slice(n-1,n).toArray.apply(0).split("\t").map(_.toInt)
    def stepTwo()={
      val topics = model.distinct
      val omega = topics.map(t => model.zipWithIndex.map(z => if(z._1!=t) 0 else bipartite.count(p => (p(cotermMST(z._2)(0)-1)!=0&p(cotermMST(z._2)(1)-1)!=0))))
        //.map(t => t.map(n => n.toFloat/t.sum.toFloat))
      val out = new PrintWriter("data\\Omega-"+n)
      omega.foreach(t => out.println(t.map(n => n.toFloat/t.sum.toFloat).mkString("\t")))
      out.flush()
      out.close()
    }
    println("stepTwo Omega Started")
    stepTwo()
    println("stepTwo Omega Finished")
    def calculateJaccardSimilarity(i: Int,j: Int):Double ={
      val aSet = coterm.foldLeft(Set(i))((z,e) => if (e(0)==i|e(1)==i) (z ++ Set(e(0),e(1))) else z)
      val bSet = coterm.foldLeft(Set(j))((z,e) => if (e(0)==j|e(1)==j) (z ++ Set(e(0),e(1))) else z)
      (aSet & bSet).size.toDouble/(aSet | bSet).size.toDouble
    }
    def calculateSimilarity(edge_a: Set[Int], edge_b: Set[Int]):Double={
      if((edge_a & edge_b).isEmpty) return  0
      if(edge_a == edge_b) return 1
      //i-k and j-k
      val i = (edge_a  diff (edge_a & edge_b)).toArray.apply(0)-1
      val j = (edge_b  diff (edge_a & edge_b)).toArray.apply(0)-1
      calculateJaccardSimilarity(i+1,j+1)
    }
    def projecting(paper: Array[Int]): Array[Set[Int]] = {
      (for (i <- paper.indexOf(1) to paper.lastIndexOf(1); j = paper.indexOf(1, i + 1);
            if (paper(i) == 1 & j > 0))
      yield Set(i + 1, j + 1)).toArray
    }
    def stepThree()={
      val topics = model.distinct
      def incrementalCal(z:Array[Int],e1:Set[Int]):Array[Int] = {
        //val edge = cotermMST.find(e => Set(e(0),e(1)) == e1)
        //if(edge != None){
          //z(topics.indexOf(model(edge.get.apply(2) - 1)))+=1
        //}else{
          //z(topics.indexOf(model(cotermMST.map(e2 => (calculateSimilarity(e1,Set(e2(0),e2(1))),e2(2))).maxBy(_._1)._2 - 1)))+=1
          val maxZ = model.zipWithIndex.foldLeft(Map(0->(0.0,0)))((s,x) => if(s.get(x._1).isEmpty) s+(x._1 -> (calculateSimilarity(e1,Set(cotermMST(x._2)(0),cotermMST(x._2)(1))),1))
          else s.updated(x._1, (s.get(x._1).get._1+calculateSimilarity(e1,Set(cotermMST(x._2)(0),cotermMST(x._2)(1))),s.get(x._1).get._2+1))).-(0).maxBy(x => x._2._1/x._2._2.toFloat)._1
          z(topics.indexOf(maxZ))+=1
        //}
        z
      }
      val theta = (for (paper <- bipartite)
      yield (projecting(paper).foldLeft(Array.ofDim[Int](topics.length))((z,e1) => incrementalCal(z,e1))))
      val out = new PrintWriter("data\\Theta-"+n)
      theta.foreach(p => out.println(p.map(t => t.toFloat/p.sum.toFloat).mkString("\t")))
      out.flush()
      out.close()
    }
    println("stepThree Theta Started")
    //stepThree()
    println("stepThree Theta Finished")
  }
  def ModelAnalysis(n:Int = 5888,t:Int = 500) = {
    val bipartite = Source.fromFile("data\\BipartiteMaxCompartMatrix").getLines.toArray
      .map(_.split("\t").map(_.toInt)).filter(x => x.count(_==1)>=2)
    val cotermMST = Source.fromFile("data\\cotermMST").getLines.toArray
      .map(_.split("\t").map(_.toInt))
    //start from 0!get the nth layer of tree as the model
    //val model = Source.fromFile("data\\LinkTopicTree").getLines.slice(n+1,n+2).toArray.apply(0).split("\t").map(_.toInt)
    //start from 0!get the tth topic of Omega
    val omega = Source.fromFile("data\\Omega-"+n).getLines
    val keywords = Source.fromFile("data\\keywords").getLines.toArray
    val omega_t = omega.slice(t-1,t).toArray.apply(0).split("\t").map(_.toFloat)
    val topic_t = cotermMST.filter(e => omega_t(e(2)-1)!=0.0).take(100)
    def stepOne_Network()={
      val out = new PrintWriter("data\\TopicsResult-"+n+"\\Topic" + t)
      topic_t.foreach(x => out.println(keywords(x(0)-1)+"\t"+keywords(x(1)-1)))
      out.flush()
      out.close()
    }
    println("stepOne_Network Started")
    //stepOne_Network()
    println("stepOne_Network Finished")
    val phi_t = (topic_t.foldLeft(Set(0))((z,x)=>z++Set(x(0),x(1))) - 0).toArray
      .map(k => (k,omega_t.zipWithIndex.filter(z => cotermMST.filter(e => e(0)==k|e(1)==k).exists(_(2)==z._2)).unzip._1.sum))
    def stepTwo_Phi()={
      val out = new PrintWriter("data\\TopicsResult-"+n+"\\Phi" + t)
      phi_t.foreach(k => out.println(keywords(k._1-1)+"\t"+k._2))
      out.flush()
      out.close()
    }
    println("stepTwo_Phi Started")
    stepTwo_Phi()
    println("stepTwo_Phi Finished")
    def stepThree_RepresentationDegree()={
      val out = new PrintWriter("data\\TopicsResult-"+n+"\\RepresentationDegree" + t)
      val degreeMST = (1 to bipartite(0).size).toArray[Int].map(k => cotermMST.filter(e => e(0)==k|e(1)==k).size)
      val RD = phi_t.map(k => (k._1,k._2 * topic_t.filter(e => e(0)==k._1|e(1)==k._1).size.toFloat / degreeMST(k._1-1).toFloat))
      RD.sortBy(_._2).foreach(x => out.println(keywords(x._1-1)+"\t"+x._2))
      out.flush()
      out.close()
    }
    println("stepThree RepresentationDegree Started")
    stepThree_RepresentationDegree()
    println("stepThree RepresentationDegree Finished")
  }
  def main(args: Array[String]): Unit = {
    //ModelGeneration()
    //ModelThetaOmegaAnalysis(81075)
    //ModelThetaOmegaAnalysis(66483)
    //ModelThetaOmegaAnalysis(23513)
    ModelAnalysis(23513,2)
    //ModelThetaOmegaAnalysis(88732)
    //ModelAnalysis(88732,353)
    /*val papers = Source.fromFile("data\\papers").getLines().toArray
    val out = new PrintWriter("data\\filterPapers")
    val bipartite = Source.fromFile("data\\BipartiteMaxCompartMatrix").getLines.toArray
      .map(_.split("\t").map(_.toInt)).zip(papers).filter(x => x._1.count(_==1)>=2)
      .foreach(x => out.println(x._2))
    out.flush()
    out.close()*/
  }
}