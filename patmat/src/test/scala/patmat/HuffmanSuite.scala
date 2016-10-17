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

  test("times simple string") {
    assert(List(('a', 4)) === times(List('a','a','a','a')))
  }

  test("times complex string") {
    assert(List(('a', 2), ('b', 2)) === times(List('a','b','b','a')))
  }

  test("times complex string different order") {
    assert(List(('a', 2), ('b', 2)) === times(List('b','a','b','a')))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList for some frequency table 2") {
    assert(makeOrderedLeafList(List(('t', 200), ('e', 1), ('x', 55))) === List(Leaf('e',1), Leaf('x',55), Leaf('t',200)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("create codeTree") {
    val codeTree = createCodeTree(List('e','l','t','a','t','a'))
    assert(codeTree.asInstanceOf[Fork].weight == 6)
    assert(codeTree.asInstanceOf[Fork].chars.length == 4)
    assert(codeTree.asInstanceOf[Fork].chars.contains('a'))
    assert(codeTree.asInstanceOf[Fork].chars.contains('e'))
    assert(codeTree.asInstanceOf[Fork].chars.contains('l'))
    assert(codeTree.asInstanceOf[Fork].chars.contains('t'))
  }

  test("decode"){
    new TestTrees {
      assert(decode(t1, List(0)) == List('a'))
      assert(decode(t1, List(1)) == List('b'))
      assert(decode(t2, List(0,0,0,1)) == List('a','b'))
      assert(decode(t2, List(0,0,0,1,1)) == List('a','b','d'))
      assert(decode(t2, List(0,0,0,1,1,0,0)) == List('a','b','d','a'))
    }
  }

  test("decode secret"){
    println(decodedSecret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abaddba".toList)) === "abaddba".toList)
    }
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) == ('a', 0 :: Nil) :: ('b', 1 :: Nil) :: Nil )
      assert(convert(t2) == ('a', 0 :: 0 :: Nil) :: ('b', 0 :: 1 :: Nil) :: ('d', 1 :: Nil) :: Nil )
    }
  }

  test("merge code tables") {
    new TestTrees {
      val a = ('a', 0 :: Nil) :: ('b', 1 :: Nil) :: Nil
      val b = ('a', 0 :: 0 :: Nil) :: ('b', 0 :: 1 :: Nil) :: ('d', 1 :: Nil) :: Nil
      assert( mergeCodeTables(a,a) == a )
      assert( mergeCodeTables(b,b) == b )
      assert( mergeCodeTables(a,b).length == 3)
      assert( mergeCodeTables(a,b).exists(p => p._1 == 'a'))
      assert( mergeCodeTables(a,b).exists(p => p._1 == 'b'))
      assert( mergeCodeTables(a,b).exists(p => p._1 == 'd'))
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quickEncode") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abaddba".toList)) === "abaddba".toList)
    }
  }

}
