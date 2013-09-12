package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times") {
    val freq = times(List('a', 'b', 'd', 'd','a', 'd', 'a', 'd'))
    assert(freq(0)._1 === 'd')
    assert(freq(0)._2 === 4)
  }
  
  test("makeOrderedLeafList") {
    val orderedList = makeOrderedLeafList(List(('a', 4), ('b', 2), ('c', 9), ('d', 1)))
    assert(orderedList(0).char === 'd')
    assert(orderedList(3).char === 'c')
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }  

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }
  
  test("quick encode") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abd".toList)) === "abd".toList)
    }
  }
}
