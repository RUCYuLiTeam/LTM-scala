import scala.io.Source
val path = "F:\\Github\\LinkTopicModel\\data\\demo"
val LTM = Source.fromFile(path.concat("\\LinkTopicTree")).getLines.toArray
  .map(_.split("\t").map(_.toInt))
LTM.foreach(x => println(x.mkString("\t")))
val coterm = Source.fromFile(path.concat("\\coterm")).getLines.toArray
  .map(_.split("\t").map(_.toInt))
def averagePartitionDensity(layer: Array[Int],coterm: Array[Array[Int]]): Double = {
  def calPartitionDensity(cluster: Int): (Double,Double) = {
    val m = layer.filter(_==cluster).length.toDouble
    val n = (coterm.filter(x => layer(x(2)-1)==cluster)
      .foldLeft(Set(0))((z,x) => z++Set(x(0),x(1))) - 0).size.toDouble
    if(n==2) (0,m) else ((m - (n - 1))/(n * (n - 1) / 2 - (n - 1)),m)
  }
  (for(cluster <- layer.distinct) yield calPartitionDensity(cluster))
    .map(x => x._1*x._2).sum/coterm.length
}
LTM.map(x => averagePartitionDensity(x,coterm))