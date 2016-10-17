package patmat

import common._
import org.scalacheck.Prop.True

/**
 * Assignment 4: Huffman coding
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



  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case t: Leaf => t.weight
    case t: Fork => t.weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case t: Leaf => List(t.char)
    case t: Fork => t.chars
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil => Nil
    case x :: xs => timesAcc(chars, Nil)
  }

  def timesAcc(chars: List[Char], quantity: List[(Char, Int)]): List[(Char, Int)] = chars match {
    case Nil => quantity
    case x :: xs => timesAcc(xs, addCharQuantity(quantity, x))
  }

  def addCharQuantity(quantity: List[(Char, Int)], char: Char ): List[(Char, Int)] = quantity match {
    case Nil => (char, 1) :: Nil
    case (a,b) :: xs => if (a== char) (char, b + 1) :: xs
                    else addCharQuantity(xs, char) :+ (a,b)
  }
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case Nil => Nil
    case x :: xs => insertOrderedLeaf(makeOrderedLeafList(xs), Leaf(x._1, x._2))
  }

  def insertOrderedLeaf(orderedLeafs:  List[Leaf], leaf: Leaf): List[Leaf] = orderedLeafs match {
    case Nil => leaf :: Nil
    case x :: xs => if (leaf.weight < x.weight) leaf :: x :: xs
                    else x :: insertOrderedLeaf(xs, leaf)
  }

  def insertOrderedCodeTree(orderedCodeTrees: List[CodeTree], codeTree: CodeTree): List[CodeTree] = orderedCodeTrees match {
    case Nil => codeTree :: Nil
    case x :: xs => if (weight(codeTree) < weight(x)) codeTree :: x :: xs
    else x :: insertOrderedCodeTree(xs, codeTree)
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case x :: Nil => true
    case other => false
  }

//  def singleton(trees: List[CodeTree]): Boolean = trees match {
//    case Nil => true
//    case x :: Nil => x match {
//      case x: Leaf => true
//      case x: Fork => false
//    }
//    case x :: xs => false
//  }


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
    case Nil => trees
    case x :: Nil => trees
    case x :: x2 :: xs => insertOrderedCodeTree(xs, makeCodeTree(x, x2))
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
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
//  def until(xxx: ???, yyy: ???)(zzz: ???): ??? = ???

    def until(f : List[CodeTree] => Boolean,
              g: List[CodeTree] => List[CodeTree])
             (trees: List[CodeTree]): List[CodeTree] = {
    if (f(trees)) trees
    else until(f, g)(g(trees))
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val charTimes = times(chars)
    val orderedLeafList = makeOrderedLeafList(charTimes)
    val x :: xs = until(singleton, combine)(orderedLeafList)
    x
  }



  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = decodeAcc(tree, tree, bits, Nil)

  def decodeAcc(root: CodeTree, tree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = tree match {
    case t: Leaf => bits match {
      case Nil => acc :+ t.char
      case other => decodeAcc(root, root, bits, acc :+ t.char)
    }
    case t :Fork => bits match {
      case 0 :: xs => decodeAcc(root, t.left, xs, acc)
      case 1 :: xs => decodeAcc(root, t.right, xs, acc)
    }
  }
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the 'frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode,secret)




  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = encodeAcc(tree, tree, text, Nil)


  def encodeAcc(root: CodeTree, tree: CodeTree, text: List[Char],  bitsAcc: List[Bit]): List[Bit] = tree match {
    case t: Leaf => text match {
      case Nil => bitsAcc
      case x :: Nil => bitsAcc
      case x :: xs => encodeAcc(root, root, xs, bitsAcc)
    }
    case t :Fork => text match {
      case Nil => bitsAcc
      case other => if (chars(t.left).contains(text.head)) encodeAcc(root, t.left, text, bitsAcc :+ 0)
                       else encodeAcc(root, t.right, text, bitsAcc :+ 1)
    }
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case Nil => Nil
    case (a,b) :: Nil => if (a == char) b else Nil
    case (a,b) :: xs  => if (a == char) b else codeBits(xs)(char)
  }
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match {
    case t: Leaf => Nil
    case t: Fork => convertAcc(t.left, 0 :: Nil) ++ convertAcc(t.right, 1 :: Nil)
  }

  def convertAcc(tree: CodeTree, bits: List[Bit]): CodeTable = tree match {
    case t: Leaf => (t.char, bits) :: Nil
    case t: Fork => convertAcc(t.left, bits :+ 0) ++ convertAcc(t.right, bits :+ 1)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = b match {
    case Nil => a
    case x :: Nil => a match {
      case Nil => b
      case y :: Nil => if (x._1 == y._1) x :: Nil else x :: y :: Nil
      case y :: ys => if (x._1 == y._1) y :: ys else x :: y :: ys
    }
    case x :: xs => a match {
      case Nil => b
      case y :: Nil => if (x._1 == y._1) x :: Nil else x :: y :: Nil
      case y :: ys => if (x._1 == y._1) y :: mergeCodeTables(xs, ys) else x :: y :: mergeCodeTables(xs, ys)
    }
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)
    quickEncode(codeTable)(text)
  }

  def quickEncode(codeTable: CodeTable)(text: List[Char]): List[Bit] = text match {
    case Nil => Nil
    case x :: Nil => codeBits(codeTable)(x)
    case x :: xs => codeBits(codeTable)(x) ++ quickEncode(codeTable)(xs)
  }
}
