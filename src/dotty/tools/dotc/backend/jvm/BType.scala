/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package dotc
package backend.jvm

object BType {

  import dotc.core.Names

  // ------------- sorts -------------

  val VOID   : Int =  0
  val BOOLEAN: Int =  1
  val CHAR   : Int =  2
  val BYTE   : Int =  3
  val SHORT  : Int =  4
  val INT    : Int =  5
  val FLOAT  : Int =  6
  val LONG   : Int =  7
  val DOUBLE : Int =  8
  val ARRAY  : Int =  9
  val OBJECT : Int = 10
  val METHOD : Int = 11

  // ------------- primitive types -------------

  val VOID_TYPE    = new BType(VOID,    ('V' << 24) | (5 << 16) | (0 << 8) | 0, 1)
  val BOOLEAN_TYPE = new BType(BOOLEAN, ('Z' << 24) | (0 << 16) | (5 << 8) | 1, 1)
  val CHAR_TYPE    = new BType(CHAR,    ('C' << 24) | (0 << 16) | (6 << 8) | 1, 1)
  val BYTE_TYPE    = new BType(BYTE,    ('B' << 24) | (0 << 16) | (5 << 8) | 1, 1)
  val SHORT_TYPE   = new BType(SHORT,   ('S' << 24) | (0 << 16) | (7 << 8) | 1, 1)
  val INT_TYPE     = new BType(INT,     ('I' << 24) | (0 << 16) | (0 << 8) | 1, 1)
  val FLOAT_TYPE   = new BType(FLOAT,   ('F' << 24) | (2 << 16) | (2 << 8) | 1, 1)
  val LONG_TYPE    = new BType(LONG,    ('J' << 24) | (1 << 16) | (1 << 8) | 2, 1)
  val DOUBLE_TYPE  = new BType(DOUBLE,  ('D' << 24) | (3 << 16) | (3 << 8) | 2, 1)

  /*
   * Returns the Java type corresponding to the given type descriptor.
   *
   * @param off the offset of this descriptor in the Names.chrs buffer.
   * @return the Java type corresponding to the given type descriptor.
   *
   * can-multi-thread
   */
  def getType(off: Int): BType = {
    var len = 0
    Names.chrs(off) match {
      case 'V' => VOID_TYPE
      case 'Z' => BOOLEAN_TYPE
      case 'C' => CHAR_TYPE
      case 'B' => BYTE_TYPE
      case 'S' => SHORT_TYPE
      case 'I' => INT_TYPE
      case 'F' => FLOAT_TYPE
      case 'J' => LONG_TYPE
      case 'D' => DOUBLE_TYPE
      case '[' =>
        len = 1
        while (Names.chrs(off + len) == '[') {
          len += 1
        }
        if (Names.chrs(off + len) == 'L') {
          len += 1
          while (Names.chrs(off + len) != ';') {
            len += 1
          }
        }
        new BType(ARRAY, off, len + 1)
      case 'L' =>
        len = 1
        while (Names.chrs(off + len) != ';') {
          len += 1
        }
        new BType(OBJECT, off + 1, len - 1)
      // case '(':
      case _ =>
        assert(Names.chrs(off) == '(')
        var resPos = off + 1
        while (Names.chrs(resPos) != ')') { resPos += 1 }
        val resType = getType(resPos + 1)
        val len = resPos - off + 1 + resType.len;
        new BType(
          METHOD,
          off,
          if (resType.hasObjectSort) {
            len + 2 // "+ 2" accounts for the "L ... ;" in a descriptor for a non-array reference.
          } else {
            len
          }
        )
    }
  }

  /* Params denote an internal name.
   *  can-multi-thread
   */
  def getObjectType(index: Int, length: Int): BType = {
    val sort = if (Names.chrs(index) == '[') ARRAY else OBJECT;
    new BType(sort, index, length)
  }

  /*
   * @param methodDescriptor a method descriptor.
   *
   * must-single-thread
   */
  def getMethodType(methodDescriptor: String): BType = {
    val n = Names.typeName(methodDescriptor)
    new BType(BType.METHOD, n.start, n.length) // TODO assert isValidMethodDescriptor
  }

  /*
   * Returns the Java method type corresponding to the given argument and return types.
   *
   * @param returnType the return type of the method.
   * @param argumentTypes the argument types of the method.
   * @return the Java type corresponding to the given argument and return types.
   *
   * must-single-thread
   */
  def getMethodType(returnType: BType, argumentTypes: Array[BType]): BType = {
    val n = Names.typeName(getMethodDescriptor(returnType, argumentTypes))
    new BType(BType.METHOD, n.start, n.length)
  }

  /*
   * Returns the Java types corresponding to the argument types of method descriptor whose first argument starts at idx0.
   *
   * @param idx0 index into chrs of the first argument.
   * @return the Java types corresponding to the argument types of the given method descriptor.
   *
   * can-multi-thread
   */
  private def getArgumentTypes(idx0: Int): Array[BType] = {
    assert(chrs(idx0 - 1) == '(', "doesn't look like a method descriptor.")
    val args = new Array[BType](getArgumentCount(idx0))
    var off = idx0
    var size = 0
    while (Names.chrs(off) != ')') {
      args(size) = getType(off)
      off += args(size).len
      if (args(size).sort == OBJECT) { off += 2 }
      // debug: assert("LVZBSCIJFD[)".contains(chrs(off)))
      size += 1
    }
    // debug: var check = 0; while (check < args.length) { assert(args(check) != null); check += 1 }
    args
  }

  /*
   * Returns the number of argument types of this method type, whose first argument starts at idx0.
   *
   * @param idx0 index into chrs of the first argument.
   * @return the number of argument types of this method type.
   *
   * can-multi-thread
   */
  private def getArgumentCount(idx0: Int): Int = {
    assert(Names.chrs(idx0 - 1) == '(', "doesn't look like a method descriptor.")
    var off  = idx0
    var size = 0
    var keepGoing = true
    while (keepGoing) {
      val car = Names.chrs(off)
      off += 1
      if (car == ')') {
        keepGoing = false
      } else if (car == 'L') {
        while (Names.chrs(off) != ';') { off += 1 }
        off += 1
        size += 1
      } else if (car != '[') {
        size += 1
      }
    }

    size
  }

  /*
   * Returns the Java type corresponding to the return type of the given
   * method descriptor.
   *
   * @param methodDescriptor a method descriptor.
   * @return the Java type corresponding to the return type of the given method descriptor.
   *
   * must-single-thread
   */
  def getReturnType(methodDescriptor: String): BType = {
    val n     = Names.typeName(methodDescriptor)
    val delta = n.pos(')') // `delta` is relative to the Name's zero-based start position, not a valid index into chrs.
    assert(delta < n.length, s"not a valid method descriptor: $methodDescriptor")
    getType(n.start + delta + 1)
  }

  /*
   * Returns the descriptor corresponding to the given argument and return types.
   * Note: no BType is created here for the resulting method descriptor,
   *       if that's desired the invoker is responsible for that.
   *
   * @param returnType the return type of the method.
   * @param argumentTypes the argument types of the method.
   * @return the descriptor corresponding to the given argument and return types.
   *
   * can-multi-thread
   */
  def getMethodDescriptor(
      returnType: BType,
      argumentTypes: Array[BType]): String =
  {
    val buf = new StringBuffer()
    buf.append('(')
    var i = 0
    while (i < argumentTypes.length) {
      argumentTypes(i).getDescriptor(buf)
      i += 1
    }
    buf.append(')')
    returnType.getDescriptor(buf)
    buf.toString()
  }

} // end of object BType

/*
 * Based on ASM's Type class. Namer's chrs is used in this class for the same purposes as the `buf` char array in asm.Type.
 *
 * All methods of this classs can-multi-thread
 */
final class BType(val sort: Int, val off: Int, val len: Int) {

  import dotc.core.Names

  /*
   * can-multi-thread
   */
  def toASMType: scala.tools.asm.Type = {
    import scala.tools.asm
    // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"
    (sort: @scala.switch) match {
      case asm.Type.VOID    => asm.Type.VOID_TYPE
      case asm.Type.BOOLEAN => asm.Type.BOOLEAN_TYPE
      case asm.Type.CHAR    => asm.Type.CHAR_TYPE
      case asm.Type.BYTE    => asm.Type.BYTE_TYPE
      case asm.Type.SHORT   => asm.Type.SHORT_TYPE
      case asm.Type.INT     => asm.Type.INT_TYPE
      case asm.Type.FLOAT   => asm.Type.FLOAT_TYPE
      case asm.Type.LONG    => asm.Type.LONG_TYPE
      case asm.Type.DOUBLE  => asm.Type.DOUBLE_TYPE
      case asm.Type.ARRAY   |
           asm.Type.OBJECT  => asm.Type.getObjectType(getInternalName)
      case asm.Type.METHOD  => asm.Type.getMethodType(getDescriptor)
    }
  }

  /*
   * Unlike for ICode's REFERENCE, isBoxedType(t) implies isReferenceType(t)
   * Also, `isReferenceType(RT_NOTHING) == true` , similarly for RT_NULL.
   * Use isNullType() , isNothingType() to detect Nothing and Null.
   *
   * can-multi-thread
   */
  def hasObjectSort = (sort == BType.OBJECT)

  /*
   * Returns the number of dimensions of this array type. This method should
   * only be used for an array type.
   *
   * @return the number of dimensions of this array type.
   *
   * can-multi-thread
   */
  def getDimensions: Int = {
    var i = 1
    while (chrs(off + i) == '[') {
      i += 1
    }
    i
  }

  /*
   * Returns the (ultimate) element type of this array type.
   * This method should only be used for an array type.
   *
   * @return Returns the type of the elements of this array type.
   *
   * can-multi-thread
   */
  def getElementType: BType = {
    assert(isArray, s"Asked for the element type of a non-array type: $this")
    BType.getType(off + getDimensions)
  }

  /*
   * Returns the internal name of the class corresponding to this object or
   * array type. The internal name of a class is its fully qualified name (as
   * returned by Class.getName(), where '.' are replaced by '/'. This method
   * should only be used for an object or array type.
   *
   * @return the internal name of the class corresponding to this object type.
   *
   * can-multi-thread
   */
  def getInternalName: String = {
    new String(chrs, off, len)
  }

  /*
   * @return the suffix of the internal name until the last '/' (if '/' present), internal name otherwise.
   *
   * can-multi-thread
   */
  def getSimpleName: String = {
    assert(hasObjectSort, s"not of object sort: $toString")
    val iname = getInternalName
    val idx = iname.lastIndexOf('/')
    if (idx == -1) iname
    else iname.substring(idx + 1)
  }

  /*
   * Returns the argument types of methods of this type.
   * This method should only be used for method types.
   *
   * @return the argument types of methods of this type.
   *
   * can-multi-thread
   */
  def getArgumentTypes: Array[BType] = {
    BType.getArgumentTypes(off + 1)
  }

  /*
   * Returns the return type of methods of this type.
   * This method should only be used for method types.
   *
   * @return the return type of methods of this type.
   *
   * can-multi-thread
   */
  def getReturnType: BType = {
    assert(chrs(off) == '(', s"doesn't look like a method descriptor: $toString")
    var resPos = off + 1
    while (chrs(resPos) != ')') { resPos += 1 }
    BType.getType(resPos + 1)
  }

  // ------------------------------------------------------------------------
  // Inspector methods
  // ------------------------------------------------------------------------

  def isPrimitiveOrVoid = (sort <  BType.ARRAY) // can-multi-thread
  def isValueType       = (sort <  BType.ARRAY) // can-multi-thread
  def isArray           = (sort == BType.ARRAY) // can-multi-thread
  def isUnitType        = (sort == BType.VOID)  // can-multi-thread

  def isRefOrArrayType   = { hasObjectSort ||  isArray    } // can-multi-thread
  def isNonUnitValueType = { isValueType   && !isUnitType } // can-multi-thread

  /* On the JVM,
   *    BOOL, BYTE, CHAR, SHORT, and INT
   *  are like Ints for the purpose of lub calculation.
   *
   * can-multi-thread
   */
  def isIntSizedType = {
    (sort : @scala.switch) match {
      case BType.BOOLEAN | BType.CHAR  |
           BType.BYTE    | BType.SHORT | BType.INT
        => true
      case _
        => false
    }
  }

  /* On the JVM, similar to isIntSizedType except that BOOL isn't integral while LONG is.
   *
   * can-multi-thread
   */
  def isIntegralType = {
    (sort : @scala.switch) match {
      case BType.CHAR  |
           BType.BYTE  | BType.SHORT | BType.INT |
           BType.LONG
        => true
      case _
        => false
    }
  }

  /* On the JVM, FLOAT and DOUBLE.
   *
   * can-multi-thread
   */
  def isRealType = { (sort == BType.FLOAT ) || (sort == BType.DOUBLE) }

  def isNumericType = (isIntegralType || isRealType) // can-multi-thread

  /* Is this type a category 2 type in JVM terms? (ie, is it LONG or DOUBLE?)
   *
   * can-multi-thread
   */
  def isWideType = (getSize == 2)

  /*
   * Element vs. Component type of an array:
   * Quoting from the JVMS, Sec. 2.4 "Reference Types and Values"
   *
   *   An array type consists of a component type with a single dimension (whose
   *   length is not given by the type). The component type of an array type may itself be
   *   an array type. If, starting from any array type, one considers its component type,
   *   and then (if that is also an array type) the component type of that type, and so on,
   *   eventually one must reach a component type that is not an array type; this is called
   *   the element type of the array type. The element type of an array type is necessarily
   *   either a primitive type, or a class type, or an interface type.
   *
   */

  /* The type of items this array holds.
   *
   * can-multi-thread
   */
  def getComponentType: BType = {
    assert(isArray, s"Asked for the component type of a non-array type: $this")
    BType.getType(off + 1)
  }

  // ------------------------------------------------------------------------
  // Conversion to type descriptors
  // ------------------------------------------------------------------------

  /*
   * @return the descriptor corresponding to this Java type.
   *
   * can-multi-thread
   */
  def getDescriptor: String = {
    val buf = new StringBuffer()
    getDescriptor(buf)
    buf.toString()
  }

  /*
   * Appends the descriptor corresponding to this Java type to the given string buffer.
   *
   * @param buf the string buffer to which the descriptor must be appended.
   *
   * can-multi-thread
   */
  private def getDescriptor(buf: StringBuffer) {
    if (isPrimitiveOrVoid) {
      // descriptor is in byte 3 of 'off' for primitive types (buf == null)
      buf.append(((off & 0xFF000000) >>> 24).asInstanceOf[Char])
    } else if (sort == BType.OBJECT) {
      buf.append('L')
      buf.append(Names.chrs, off, len)
      buf.append(';')
    } else { // sort == ARRAY || sort == METHOD
      buf.append(Names.chrs, off, len)
    }
  }

  // ------------------------------------------------------------------------
  // Corresponding size and opcodes
  // ------------------------------------------------------------------------

  /*
   * Returns the size of values of this type.
   * This method must not be used for method types.
   *
   * @return the size of values of this type, i.e., 2 for <tt>long</tt> and
   *         <tt>double</tt>, 0 for <tt>void</tt> and 1 otherwise.
   *
   * can-multi-thread
   */
  def getSize: Int = {
    // the size is in byte 0 of 'off' for primitive types (buf == null)
    if (isPrimitiveOrVoid) (off & 0xFF) else 1
  }

  /*
   * Returns a JVM instruction opcode adapted to this Java type. This method
   * must not be used for method types.
   *
   * @param opcode a JVM instruction opcode. This opcode must be one of ILOAD,
   *        ISTORE, IALOAD, IASTORE, IADD, ISUB, IMUL, IDIV, IREM, INEG, ISHL,
   *        ISHR, IUSHR, IAND, IOR, IXOR and IRETURN.
   * @return an opcode that is similar to the given opcode, but adapted to
   *         this Java type. For example, if this type is <tt>float</tt> and
   *         <tt>opcode</tt> is IRETURN, this method returns FRETURN.
   *
   * can-multi-thread
   */
  def getOpcode(opcode: Int): Int = {
    import scala.tools.asm.Opcodes
    if (opcode == Opcodes.IALOAD || opcode == Opcodes.IASTORE) {
      // the offset for IALOAD or IASTORE is in byte 1 of 'off' for
      // primitive types (buf == null)
      opcode + (if (isPrimitiveOrVoid) (off & 0xFF00) >> 8 else 4)
    } else {
      // the offset for other instructions is in byte 2 of 'off' for
      // primitive types (buf == null)
      opcode + (if (isPrimitiveOrVoid) (off & 0xFF0000) >> 16 else 4)
    }
  }

  // ------------------------------------------------------------------------
  // Equals, hashCode and toString
  // ------------------------------------------------------------------------

  /*
   * Tests if the given object is equal to this type.
   *
   * @param o the object to be compared to this type.
   * @return <tt>true</tt> if the given object is equal to this type.
   *
   * can-multi-thread
   */
  override def equals(o: Any): Boolean = {
    if (!(o.isInstanceOf[BType])) {
      return false
    }
    val t = o.asInstanceOf[BType]
    if (this eq t) {
      return true
    }
    if (sort != t.sort) {
      return false
    }
    if (sort >= BType.ARRAY) {
      if (len != t.len) {
        return false
      }
      // sort checked already
      if (off == t.off) {
        return true
      }
      var i = 0
      while (i < len) {
        if (Names.chrs(off + i) != Names.chrs(t.off + i)) {
          return false
        }
        i += 1
      }
      // If we reach here, we could update the largest of (this.off, t.off) to match the other, so as to simplify future == comparisons.
      // But that would require a var rather than val.
    }
    true
  }

  /*
   * @return a hash code value for this type.
   *
   * can-multi-thread
   */
  override def hashCode(): Int = {
    var hc = 13 * sort;
    if (sort >= BType.ARRAY) {
      var i = off
      val end = i + len
      while (i < end) {
        hc = 17 * (hc + Names.chrs(i))
        i += 1
      }
    }
    hc
  }

  /*
   * @return the descriptor of this type.
   *
   * can-multi-thread
   */
  override def toString: String = { getDescriptor }

}
