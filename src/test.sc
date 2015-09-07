import java.io.PrintWriter
import scala.io.Source
//bipartite matrix
val bipartite =
  Source.fromFile("F:\\Github\\LinkTopicModel\\data\\demo\\bipartite").getLines.toArray
    .map(_.split("\t").map(_.toInt))
(for (row <- 0 to bipartite.length - 1;
      col <- 0 to bipartite(row).length - 1;
      if (bipartite(row)(col) != 0)) yield (row + 1, col + 1, bipartite(row)(col))).foreach(println(_))
//coterm network
val coterm =
  (for (paper <- bipartite;
        i <- paper.indexOf(1) to paper.lastIndexOf(1); j = paper.indexOf(1, i + 1);
        if (paper(i) == 1 & j > 0)) yield (i + 1, j + 1)).distinct
    .zipWithIndex.map(x => Array(x._1._1, x._1._2, x._2 + 1))
coterm.foreach(x => println(x.mkString("\t")))
var out = new PrintWriter("F:\\Github\\LinkTopicModel\\data\\demo\\coterm")
coterm.foreach(x => out.println(x.mkString("\t")))
out.flush()
out.close()
//克鲁斯卡尔算法
def extractMaximumSpanningTree(bipartite: Array[Array[Int]], paper: Array[Int]): Array[Array[Int]] = {
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
val maximumSpanningTreePaperCoterm =
  (for (paper <- bipartite;edge <- extractMaximumSpanningTree(bipartite,paper)) yield (edge(0),edge(1))).distinct
    .zipWithIndex.map(x => Array(x._1._1, x._1._2, x._2 + 1))
maximumSpanningTreePaperCoterm.foreach(x => println(x.mkString("\t")))
out = new PrintWriter("F:\\Github\\LinkTopicModel\\data\\demo\\cotermMST")
maximumSpanningTreePaperCoterm.foreach(x => out.println(x.mkString("\t")))
out.flush()
out.close()
//similarity
val alpha = 0.2
val beta = 0.05
def calculateSimilarityofAllEdges(bipartite: Array[Array[Int]], edges: Array[Array[Int]]): Map[Set[Int], Double] = {
  def calculateSimilarity(edge_a: Set[Int], edge_b: Set[Int]): Double = {
    if ((edge_a & edge_b).isEmpty) return 0
    if (edge_a == edge_b) return 1
    //P(ij|k)
    val i = (edge_a diff (edge_a & edge_b)).toArray.apply(0) - 1
    val j = (edge_b diff (edge_a & edge_b)).toArray.apply(0) - 1
    val k = (edge_a & edge_b).toArray.apply(0) - 1
    val N_1 = bipartite.count(x => (x(i) != 0 & x(j) != 0 & x(k) != 0))
    val N_2 = Array(bipartite.count(x => (x(i) != 0 & x(j) != 0)),
      bipartite.count(x => (x(i) != 0 & x(k) != 0)),
      bipartite.count(x => (x(j) != 0 & x(k) != 0))).min
    val N_3 = Array(bipartite.count(x => (x(i) != 0)),
      bipartite.count(x => (x(k) != 0)),
      bipartite.count(x => (x(j) != 0))).min
    return ((1 - alpha) * N_1 + (alpha - beta) * N_2 + beta * N_3) / bipartite.count(_(k) != 0)
  }
  return ((for (edge_a <- edges; edge_b <- edges.filter(_(2) > edge_a(2));
                sim = calculateSimilarity(edge_a.take(2).toSet, edge_b.take(2).toSet); if (sim != 0))
  yield (Set(edge_a(2), edge_b(2)) -> sim)).toMap)
}
val similarity = calculateSimilarityofAllEdges(bipartite, coterm)
out = new PrintWriter("F:\\Github\\LinkTopicModel\\data\\demo\\similarity")
similarity.foreach(x => out.println(x._1.mkString("\t").concat("\t").concat(x._2.toString)))
out.flush()
out.close()
val sorted = similarity.groupBy(_._2).toList.sortWith((a,b) => a._1 > b._1).map(_._2.keySet)
sorted.foreach(println)
//LinkCommunity
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
generateLinkCommunityTree(coterm, sorted,"F:\\Github\\LinkTopicModel\\data\\demo\\LinkTopicTree")