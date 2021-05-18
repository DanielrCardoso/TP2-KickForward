package br.unb.cic

object Hello{

  def read_file(path: String, func: (String, (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {    
    func(scala.io.Source.fromFile(path).mkString, normalize)
  }

  def filter_chars(data: String, func: (String, (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {    
    func(data.replaceAll("[^A-Za-z0-9 ]", " "), scan)
  }

  def normalize(data: String, func: (String, (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {    
    func(data.toLowerCase(), remove_stop_words)
  }

  def scan(data: String, func: (Array[String], (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {    
    val word_list = data.split(" +")
    func(word_list, frequencies)
  }

  def remove_stop_words(data: Array[String], func: (List[String], (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit) => Unit): Unit = {    
    var stop_words = scala.io.Source.fromFile("/home/felipefon/Desktop/Projetos/UnB/TP2/ed/src/main/scala/stop_words.txt").mkString
    stop_words = stop_words.replaceAll("[^A-Za-z0-9 ]", " ")
    val stop_words_list = stop_words.split(" +")
    var new_data = List.empty[String]

    for (i <- data) {
      var constant_var = true
      for (j <- stop_words_list) {
        if (i == j) {
          constant_var = false
        }
      }
      if (constant_var == true){
        new_data = new_data ++ List(i)
      }
    }

    func(new_data, sort)
  }

  def frequencies(data: List[String], func: (Seq[(String, Int)], (Seq[(String, Int)], (() => Unit) => Unit) => Unit) => Unit): Unit = {    
    func(data.groupBy(identity).mapValues(_.size).toSeq, print_text)
  }

  def sort(data: Seq[(String, Int)], func: (Seq[(String, Int)], (() => Unit) => Unit) => Unit): Unit = {    
    func(data.sortWith(_._2 > _._2), no_op)
  }

  def print_text(data: Seq[(String, Int)], func: (() => Unit) => Unit): Unit = {
    for (w <- data) println(w._1 + " - " + w._2)
    func(null)
  }

  def no_op(func: () => Unit): Unit = {
    return
  }

  def main(args: Array[String]) = {
    read_file("/home/felipefon/Desktop/Projetos/UnB/TP2/ed/src/main/scala/input.txt", filter_chars)
  }
}