//(compare: Int -> Boolean)
// quick sorting
def quickSort(xs: List[Int]): List[Int] = {
  if (xs.isEmpty) xs
  else
    quickSort(xs.filter(x=>x<xs.head)):::xs.head::quickSort(xs.filter(x=>x>xs.head))
}
List(10,2,33,29,19,20,11)
quickSort(List(10,2,33,29,19,20,11))
//heap sorting
def heapSort(xs: Array[Int]): Array[Int] = {
  def maxHeapAdjust(parent:Int,maxn: Int):Unit={
    val left = ((parent + 1) << 1) - 1
    val right = (parent + 1) << 1
    var largest = parent
    if(left < maxn && xs(parent) < xs(left)){
      largest = left
    }
    if(right < maxn && xs(parent) < xs(right)){
      largest = right
    }
    if(largest!=parent){
      swap(largest,parent)
      maxHeapAdjust(largest,maxn)
    }
  }
  def swap(i:Int,j:Int)={
    val tmp = xs(i)
    xs(i) = xs(j)
    xs(j) = tmp
  }
  //build heap
  for(i <- (0 to xs.size - 1).reverse;parent = (i-1) >> 1;if(parent >= 0)){
    maxHeapAdjust(parent,xs.size - 1)
  }
  println(xs.mkString(" "))
  //sort
  for(i <- (1 to xs.size - 1).reverse){
    swap(i,0)
    maxHeapAdjust(0,i)
  }
  xs
}
Array(10,33,2,29,18,20,19)
heapSort(Array(10,33,2,29,18,20,19))
//merge sorting
def mergeSort(xs: List[Int]): List[Int] = {
  def merge(xs1:List[Int],xs2:List[Int]): List[Int]={
    var i,j = 0
    var x = List[Int]()
    while (i<xs1.size && j<xs2.size){
      x = x ::: List(xs1(i) min xs2(j))
      if(xs1(i) < xs2(j)){
        i += 1
      }else{
        j += 1
      }
    }
    if (i<xs1.size){
      x = x:::xs1.takeRight(xs1.size - i)
    }else{
      x = x:::xs2.takeRight(xs2.size - j)
    }
    x
  }
  def sort(start: Int,end: Int):List[Int] ={
    val x = xs.slice(start,end + 1)
    if(start >= end) return x
    val mid = (start+end)/2
    merge(sort(start,mid),sort(mid + 1,end))
  }
  sort(0, xs.size - 1)
}
List(10,33,2,29,18,20,19)
mergeSort(List(10,33,2,29,18,20,19))