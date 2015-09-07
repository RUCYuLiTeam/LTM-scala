import java.io.PrintWriter

import scala.io.Source

/**
 * Created by Joshua on 2015/4/3.
 */
object LinkTopicModelExample {
  val path = "F:\\Github\\LinkTopicModel\\data\\example"
  def main(args: Array[String]): Unit = {
    val bipartite = Source.fromFile(path.concat("\\BipartiteMaxCompartMatrix")).getLines.toArray
      .map(_.split("\t").map(_.toInt)).filter(x => x.count(_==1)>=2)

    val coterm = Source.fromFile(path.concat("\\coterm")).getLines.toArray
      .map(_.split(" ").map(_.toInt))

    //similarity
    val alpha = 0.20
    val beta = 0.05
    def calculateSimilarityofAllEdges(bipartite: Array[Array[Int]], edges: Array[Array[Int]]): Map[Set[Int], Double] = {
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
    val similarity = calculateSimilarityofAllEdges(bipartite, coterm)
    val out = new PrintWriter(path.concat("\\similarity"))
    similarity.foreach(x => out.println(x._1.mkString("\t").concat("\t").concat(x._2.toString)))
    out.flush()
    out.close()
    val sorted = similarity.groupBy(_._2).toList.sortWith((a,b) => a._1 > b._1).map(_._2.keySet)
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
    generateLinkCommunityTree(coterm, sorted,path.concat("\\LinkTopicTree"))
  }
}
