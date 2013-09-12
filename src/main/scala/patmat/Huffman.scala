package patmat

import common._

/**
 * Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  
  def weight(tree: CodeTree): Int = tree match {
    case Fork(l, r, c, w) => w
    case Leaf(c, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(l, r, c, w) => c
    case Leaf(c, w) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Generating Huffman trees

  /**
   * This function allows to easily create a character list
   * from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the 
   * list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting  
   * list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs,
   * where each pair consists of a character and an integer. 
   * Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, the 
   * accessors `_1` and `_2` are used:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern 
   * matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */  
  def times(chars: List[Char]): List[(Char, Int)] = {
    def acc(chars: List[Char], pairs: List[(Char, Int)]): List[(Char, Int)] = chars match {
      case List() => pairs
      case head::tail => acc(chars.filter(c => c != head), (head, chars.count(x => x == head))::pairs)        
    }
    
    acc(chars, List())  
  }
    
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def acc_leaves(freqs: List[(Char, Int)], leaves: List[Leaf]): List[Leaf] = freqs match {
    case List() => leaves
    case head::tail => acc_leaves(tail, Leaf(head._1, head._2)::leaves)
  }   
  
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    acc_leaves(freqs, List()).sortWith((l1, l2) => weight(l1) < weight(l2))
    
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case head::Nil => true    
    case _ => false    
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */  
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case lowest::nextLowest::tail => (makeCodeTree(lowest, nextLowest)::tail).sortBy(t => weight(t))
    case _ => trees
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   */
   
  def until(singleTree: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree = singleTree(trees) match {
    case true => trees.head
    case false => until(singleTree, merge)(merge(trees))
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))

  // Part 3: Decoding

  type Bit = Int  

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeTree(rtree: CodeTree, rbits: List[Bit], acc: List[Char]): List[Char] = (rtree, rbits) match {
        case (Leaf(c, w), Nil) => acc:::List(c)
        case (Leaf(c, w), rrbits) => decodeTree(tree, rrbits, acc:::List(c))
        case (Fork(l, r, c, w), 0::tail) => decodeTree(l, tail, acc)
        case (Fork(l, r, c, w), 1::tail) => decodeTree(r, tail, acc)  
        case (_, _) => acc
    }
    decodeTree(tree, bits, List())
  } 


  // Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def accBits(rtree: CodeTree, rtext: List[Char], bits: List[Bit]): List[Bit] = (rtree, rtext) match {  
      //case (Leaf(_,_), head::tail) => accBits(tree, tail, )
      case (Fork(l, r, c, w), head::tail) => (chars(l).contains(head)) match {
        case true => l match {
          case Leaf(c, w) => accBits(tree, rtext.tail, bits:::List(0))
          case Fork(ll, rr, c, w) => accBits(l, rtext, bits:::List(0))
        }
        case false => r match {
          case Leaf(c, w) => accBits(tree, rtext.tail, bits:::List(1))
          case Fork(ll, rr, c, w) => accBits(r, rtext, bits:::List(1))
        }
      }
      case (_, _) => bits      
    }
    
    accBits(tree, text, List())
  }


  // Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case (character, bits)::tail => if (character == char) bits else codeBits(tail)(char)
    case _ => Nil
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   */
   def convert(tree: CodeTree): CodeTable = {
      def convertToTable(tree: CodeTree, bits: List[Bit]): CodeTable = tree match {
        case Leaf(c, w) => List((c, bits))
        case Fork(l, r, c, w) => mergeCodeTables(convertToTable(l, bits:::List(0)), convertToTable(r, bits:::List(1)))
      }
      convertToTable(tree, List())
   }
   

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a:::b

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  /*def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeBits(codeTable: CodeTable, text: List[Char], accBits: List[Bit]): List[Bit] = text match {
      case Nil => accBits
      case head::tail => encodeBits(codeTable, tail, accBits:::codeBits(codeTable)(head))
    }    
    encodeBits(convert(tree), text, List())
  }*/
  
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeBits(codeTable: CodeTable)(text: List[Char])(accBits: List[Bit]): List[Bit] = text match {
      case Nil => accBits
      case head::tail => encodeBits(codeTable)(tail)(accBits:::codeBits(codeTable)(head))
    }     
    encodeBits(convert(tree))(text)(List())
  }
}
