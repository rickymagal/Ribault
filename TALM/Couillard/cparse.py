#  ---------------------------------------------------------------
#  cparse.py
#
#  Atul Varma
#  Python C Compiler - Parser
#  $Id: cparse.py,v 1.2 2004/05/27 16:25:08 varmaa Exp $
#  ---------------------------------------------------------------

import yacc

from clex import tokens

#  ---------------------------------------------------------------
#  ABSTRACT SYNTAX TREE - NODES
#  ---------------------------------------------------------------

class Node:
    "Base class for all nodes on the abstract syntax tree."
    
    def is_null(self):
        """Returns whether the node represents a null node."""
        
        return 0

    def is_const(self):
        """Returns whether the node is a constant numeric number
        (e.g., "5")."""
        
        return 0
    
    def has_address(self):
        """Returns whether the node has an address (i.e., is a valid
        lvalue)."""
        
        return "has_addr" in self.__dict__

    def set_has_address(self):
        """Tells the node that has an address (is an lvalue).
        Ultimately, the address of the node should be placed in the
        output_addr attribute."""
        
        self.has_addr = 1
        self.output_addr = 0

    def calculate(self):
        """Calculates the constant numeric value of the node and
        its subnodes, if one exists.  For instance, if a node
        corresponds to the expression "5+3", then this method
        would return 8."""
        
        return None
    
    def accept(self, visitor):
        """Accept method for visitor classes (see cvisitor.py)."""
        
        return self._accept(self.__class__, visitor)
        
    def _accept(self, klass, visitor):
        """Accept implementation.  This is actually a recursive
        function that dynamically figures out which visitor method to
        call.  This is done by appending the class' name to 'v', so if
        the node class is called MyNode, then this method tries
        calling visitor.vMyNode().  If that node doesn't exist, then
        it recursively attempts to call the visitor method
        corresponding to the class' superclass (e.g.,
        visitor.vNode())."""
        
        visitor_method = getattr(visitor, "v%s" % klass.__name__, None)
        if visitor_method == None:
            bases = klass.__bases__
            last = None
            for i in bases:
                last = self._accept(i, visitor)
            return last
        else:
            return visitor_method(self)

class NullNode(Node):
    """A null node is like a null terminator for AST's."""

    def __init__(self):
        self.type = 'void'

    def is_null(self):
        return 1

class ArrayExpression(Node):
    """This is an expression with array notation, like "a[5+b]"."""
    
    def __init__(self, expr, index):
        self.expr = expr
        self.index = index

class StringLiteral(Node):
    """A string literal, e.g. the string "Hello World" in
    printf("Hello World")."""
    
    def __init__(self, str):
        self._str = str
        self.type = PointerType(BaseType('char'))

    def append_str(self, str):
        self._str += str
    
    def get_str(self):
        return self._str
    
    def get_sanitized_str(self):
        """Returns a 'sanitized' version of the string, converting
        all carriage returns to '\n' symbols, etc."""

        return self._str.replace('\n', '\\n')

class Id(Node):
    """An identifier, which can correspond to the name of
    a function, variable, etc..."""

    def __init__(self, name, lineno, index=-1, s=False, a=False, op="", val=0, local=False, starter=False, last=False):
        self.name = name
        self.lineno = lineno
        self.index = index
        self.s = s
        self.a = a
        self.op = op
        self.val = val
        self.local = local
        self.starter = starter
        self.last = last

    def set_index(self, index):
        self.index = index

    def set_all(self):
        self.a = True

    def set_mytid(self):
        self.s = True

    def set_last(self):
        self.last = True

    def set_mytid_expr(self, op, val):
        self.op = op
        self.val = val

class Const(Node):
    """A numeric constant (i.e., an integral literal), such as
    the number 5."""
    
    def __init__(self, value, type):
        self.value = value
        print(self.value)
        self.type = type

    def calculate(self):
        return self.value

    def is_const(self):
        return 1

def _get_calculated(node):
    """Attempts to calculate the numeric value of the expression,
    returning a Const node if it was able to convert the expression.
    If the expression isn't a constant expression like "5+3", then
    this function just returns the node unmodified."""
    typedic={"<class 'int'>":"int", "<class 'float'>":"float"}
    result = node.calculate()
    if result != None and str(type(result)) in typedic:
        return Const(result, BaseType(typedic[str(type(result))]))
    else:
        return node

class Unaryop(Node):
    """Any generic unary operator.  This is an abstract base class."""
    
    def __init__(self, node):
        self.expr = node

class Negative(Unaryop):
    """A negative unary operator, e.g. '-5'."""
    
    def calculate(self):
        val = self.expr.calculate()
        if val != None:
            return -val
        return None

class Pointer(Unaryop):
    """A pointer dereference, e.g. '*a'."""

    pass

class BinNot(Unaryop):
    """A binary not (tilde) unary operator, e.g. '~5'."""
    
    def calculate(self):
        val = self.expr.calculate()
        if val != None:
            return ~val
        return None

class AddrOf(Unaryop):
    """An address-of operator, e.g. '&a'."""
    
    pass

class Binop(Node):
    """Any binary operator, such as that for arithmetic operations
    (+/-/*), assignment operations (=/+=/-=), and so forth."""

    # List of assignment operators.
    ASSIGN_OPS = ['=', '+=', '-=', '*=', '/=', '%=', '|=', '&=', '^=', '<<=', '>>=']
    
    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def calculate(self):
        left = self.left.calculate()
        right = self.right.calculate()
        if left != None and right != None:
            x = eval("%s %s %s" % (str(left), self.op, str(right)))
            return x
        else:
            return None

class IfStatement(Node):
    """An if/then/else statement (Else is optional)."""
    
    def __init__(self, expr, then_stmt, else_stmt=None):
        if else_stmt == None:
            else_stmt = NullNode()
        self.expr = expr
        self.then_stmt = then_stmt
        self.else_stmt = else_stmt

class SwitchStatement(Node):
    """A Switch statement."""
    
    def __init__(self, expr, stmt):
        self.expr = expr
        self.stmt = stmt

class CaseStatement(Node):
    """A case statement."""
    
    def __init__(self, stmt, const=None):
        if const == None:
            const = NullNode()
        self.const = const
        self.stmt = stmt

class DefaultStatement(CaseStatement):
    """A Default case statement."""
    
    def __init__(self, stmt):
        CaseStatement.__init__(self, stmt)


class BreakStatement(Node):
    """A break statement (used while in a loop structure to bust out
    of it)."""

    pass

class ContinueStatement(Node):
    """A continue statement (used while in a loop structure to bust
    back to the beginning of it)."""
    
    pass

class ReturnStatement(Node):
    """A return statement, used to exit a function and optionally
    return a value."""
    
    def __init__(self, expr):
        self.expr = expr

class ForLoop(Node):
    """A for loop."""
    
    def __init__(self, begin_stmt, expr, end_stmt, stmt):
        self.expr = expr
        self.stmt = stmt
        self.begin_stmt = begin_stmt
        self.end_stmt = end_stmt

class WhileLoop(Node):
    """A while loop."""
    
    def __init__(self, expr, stmt):
        self.expr = expr
        self.stmt = stmt

class DoWhileLoop(Node):
    """A while loop."""
    
    def __init__(self, expr, stmt):
        self.expr = expr
        self.stmt = stmt

class NodeList(Node):
    """A list of nodes.  This is an abstract base class."""
    
    def __init__(self, node=None):
        self.nodes = []
        if node != None:
            self.nodes.append(node)

    def add(self, node):
        self.nodes.append(node)

class ArgumentList(NodeList):
    """A list of arguments for a function expression.  e.g., the list
     '5,2,3' in 'a = my_func(5,2,3)'."""
    
    pass

class ParamList(NodeList):
    """A list of parameters for a function prototype, e.g. the list
     'int a, char b, char c' in 'int my_func(int a, char b, char c)'."""

    def __init__(self, node=None):
        NodeList.__init__(self, node)
        self.has_ellipsis = 0

class StatementList(NodeList):
    """Any list of statements.  For instance, this can be the list of
    statements in a function body."""

    pass

class IdentifierList(NodeList):
    """A list of identifiers."""

    pass

class TranslationUnit(NodeList):
    """A list of nodes representing the program itself."""

    pass

class DeclarationList(NodeList):
    """A list of variable declarations, such as the ones put
    at the beginning of a compound statement (e.g., the beginning
    of a function body)."""
    
    pass

class InitializerList(NodeList):
    """A list of Initializer, such as the ones put
    at an array."""
    
    pass

class FunctionExpression(Node):
    """An execution of a function, e.g. 'my_func(a,b,c)'."""
    
    def __init__(self, function, arglist):
        self.function = function
        self.arglist = arglist

class CompoundStatement(Node):
    """A compound statement, e.g. '{ int i; i += 1; }'."""
    
    def __init__(self, declaration_list, statement_list):
        self.declaration_list = declaration_list
        self.statement_list = statement_list

class FunctionDefn(Node):
    """A node representing a function definition (its declaration
    and body)."""
    
    def __init__(self, declaration, body):
        self.type = declaration.type
        self.name = declaration.name
        #self.extern = declaration.extern
        #self.static = declaration.static
        self.body = body

class Declaration(Node):
    """A node representing a declaration of a function or
    variable."""
    
    def __init__(self, name, type=None):
        if type == None:
            type = NullNode()
        self.type = type
        self.name = name
        self.is_used = 0
        self.is_initialized = 0

    def set_base_type(self, type):
        if self.type.is_null():
            self.type = type
        else:
            self.type.set_base_type(type)

    def add_type(self, type):
        type.set_base_type(self.type)
        self.type = type

    def initialize(self, init, linenum):
        i = Id(self.name, linenum)
        self.init = Binop(i, init, '=')
        self.is_initialized = 1

class SuperInstruction(Node):
    """A super-instruction statement."""
    def __init__(self, body, inp=None, out=None):
            self.inp = inp
            self.out = out
            self.body = body

class ParInstruction(SuperInstruction):
    """A parallel super-instruction."""
    pass

class ParInstruction(SuperInstruction):
    """A parallel super-instruction."""
    pass

class Block(Node):
    """A block statement."""
    def __init__(self, body):
            self.body = body
        
#  ---------------------------------------------------------------
#  ABSTRACT SYNTAX TREE - TYPE SYSTEM
#  ---------------------------------------------------------------

class Type(Node):
    """A node representing the type of another node.  For instance,
    the Binop node representing '5 + a', where a is an int, will have
    a Type node associated with it that represents the fact that
    the result of the Binop is an int.

    Types can also be nested, so that for instance you can have
    a type like 'pointer(pointer(int))' which represents a
    double-pointer to an int.

    This is an abstract base class."""
    
    def __init__(self, child=None):
        if child == None:
            child = NullNode()
        self.child = child
        self.qualifiers = QualifierList()
        self.storageclass = StorageClassList()

    def set_base_type(self, type):
        """Set the base (innermost) type of a type.  For instance,
        calling this with a pointer(int) type on a pointer() type
        will give you a pointer(pointer(int))."""
        
        if self.child.is_null():
            self.child = type
        else:
            self.child.set_base_type(type)

    def get_base_type(self):
        """Get the base (innermost) type of a type."""
        if self.child.is_null():
            if isinstance(self, BaseType):
                return self.get_string()
            else:
                return "none"
        else:
            self.child.get_base_type()

    def get_string(self):
        """Return a string corresponding to the type, e.g.
        'pointer(pointer(int))'."""
        
        raise NotImplementedError()

    def get_dec_string(self):
        """Return a string corresponding to the type declaration, e.g.
        'int**'."""
        
        raise NotImplementedError()

    def get_outer_string(self):
        """Return only the outermost type of a type.  e.g.,
        calling this on a pointer(pointer(int)) type will
        return 'pointer'."""
        
        raise NotImplementedError()

    def is_function(self):
        """Returns whether or not this type represents a
        function."""
        
        return 0

    def is_array(self):
        """Returns whether or not this type represents an 
        array."""
        
        return 0

class BaseType(Type):
    """A base type representing ints, chars, etc..."""
    
    def __init__(self, type_str, child=None):
        Type.__init__(self, child)
        self.type_str = type_str

    def get_string(self):
        return self.type_str

    def get_dec_string(self):
        return self.type_str

    def get_outer_string(self):
        return self.type_str

class TypeModifier(Type):
    """A type modifier representing long, short, signed etc..."""
    
    def __init__(self, type_str, child=None):
        Type.__init__(self, child)
        self.type_str = type_str
   
    def get_string(self):
        return "%s %s" % (self.type_str,self. child.get_string())

    def get_dec_string(self):
        return "%s %s" % (self.type_str,self. child.get_dec_string())

    def get_outer_string(self):
        return self.type_str

class QualifierList(NodeList):
    def get_string(self):
        s = ""
        for q in self.nodes:
                s+=q.name+" "
        return "%s" % s

class Qualifier(Node):
    """A node representing a qualifier of a type.
    This is an abstract class."""

    def __init__(self, qualifier):
        self.name = qualifier


class ConstQualifier(Qualifier):
        pass


class VolatileQualifier(Qualifier):
        pass

class StorageClassList(NodeList):
    def get_string(self):
        s = ""
        for q in self.nodes:
                s+=q.name+" "
        return "%s" % s

class StorageClass(Node):
    """A node representing a storageclass of a type.
    This is an abstract class."""

    def __init__(self, name):
        self.name = name

class TypedefStorageClass(StorageClass):
        pass

class ExternStorageClass(StorageClass):
        pass

class StaticStorageClass(StorageClass):
        pass

class AutoStorageClass(StorageClass):
        pass

class RegisterStorageClass(StorageClass):
        pass

class ParoutStorageClass(StorageClass):
        pass

class ArrayType(Type):
    """A type representing an array"""

    def __init__(self, size, child=None):
        Type.__init__(self, child)
        self.dim = 1
        self.dim_sizes = [ size ]

    def expand(self, size):
        self.dim+=1
        self.dim_sizes.append(size)

    def get_string(self):
        dim_str = ""
        for dim in reversed(self.dim_sizes):
            dim_str += "[" + str(dim.value) + "]"
        return "%s array %s" % (self.child.get_string(), dim_str)

    def get_dec_string(self):
        dim_str = ""
        for dim in reversed(self.dim_sizes):
            dim_str += "[" + str(dim.value) + "]"
        return "%s array %s" % (self.child.get_dec_string(), dim_str)

    def get_outer_string(self):
        return 'array'

    def is_array(self):
        return 1

    def get_return_type(self):
        """Returns the return type of the array.  Internally,
        this is stored as the nested type within the array."""       
        return self.child

    def set_base_type(self, type):
        """Set the base (innermost) type of a type.  For instance,
        calling this with a pointer(int) type on a pointer() type
        will give you a pointer(pointer(int)). In the case of an 
        array, we need to expand the number of dimensions"""
        
        if not(isinstance(type, NullNode)) and type.is_array():
                self.dim+=1
                self.dim_sizes.append(type.dim_sizes[0])
        else:
                Type.set_base_type(self, type)

class FunctionType(Type):
    """A type representing a function (for function prototypes and
    function calls)."""
    
    def __init__(self, params=None, child=None):
        Type.__init__(self, child)
        if (params == None):
            params = NullNode()
        self.params = params

    def get_string(self):
        param_str = ""
        for param in self.params.nodes:
            param_str += "," + param.type.get_string()
        return "function(%s)->%s" % (param_str[1:], self.child.get_string())

    def get_dec_string(self):
        param_str = ""
        for param in self.params.nodes:
            param_str += "," + param.type.get_dec_string()
        return "function(%s)->%s" % (param_str[1:], self.child.get_dec_string())

    def get_outer_string(self):
        return 'function'

    def is_function(self):
        return 1

    def get_return_type(self):
        """Returns the return type of the function.  Internally,
        this is stored as the nested type within the function."""
        
        return self.child

    def get_params(self):
        """Returns the list of parameters for the function."""
        
        return self.params

class PointerType(Type):
    """A type representing a pointer to another (nested) type."""
    
    def get_string(self):
        return "pointer(%s)" % self.child.get_string()

    def get_dec_string(self):
        return "%s*" % self.child.get_dec_string()

    def get_outer_string(self):
        return 'pointer'

#  ---------------------------------------------------------------
#  PARSER GRAMMAR / AST CONSTRUCTION
#
#  The only thing the yacc grammar rules do is create an
#  abstract syntax tree.  Actual symbol table generation,
#  type checking, flow control checking, etc. are done by
#  the visitor classes (see cvisitors.py).
#  ---------------------------------------------------------------

# Precedence for ambiguous grammar elements.
precedence = (
    ('right', 'ELSE'),
)

class ParseError(Exception):
    "Exception raised whenever a parsing error occurs."

    pass

def p_translation_unit_01(t):
     '''translation_unit : external_declaration'''
     t[0] = TranslationUnit(t[1])

def p_translation_unit_02(t):
     '''translation_unit : translation_unit external_declaration'''
     t[1].add(t[2])
     t[0] = t[1]

def p_external_declaration(t):
     '''external_declaration : function_definition
                            | declaration
                            | block_statement'''
     t[0] = t[1]


def p_function_definition_01(t):
     '''function_definition : declaration_specifiers declarator compound_statement'''
     t[2].set_base_type(t[1])
     t[0] = FunctionDefn(t[2], t[3])

def p_function_definition_02(t):
     '''function_definition : declarator compound_statement'''
     t[0] = FunctionDefn(t[1], t[2])


#---------- LIMEI ESSES 2 PQ EH OLD STYLE ----------------------------
#def p_function_definition_03(t):
#     '''function_definition : declaration_specifiers declarator declaration_list compound_statement'''

#def p_function_definition_04(t):
#     '''function_definition : declarator declaration_list compound_statement'''
#-------------------------------------------------------------------


def p_declaration_specifiers_01(t):
     '''declaration_specifiers : storage_class_specifier'''
     myt = Type()
     myt.storageclass = StorageClassList(t[1])
     t[0] = myt

def p_declaration_specifiers_02(t):
     '''declaration_specifiers : storage_class_specifier declaration_specifiers'''
     t[2].storageclass.add(t[1])
     t[0] = t[2]   

def p_declaration_specifiers_03(t):
     '''declaration_specifiers : type_specifier'''
     t[0] = t[1]

def p_declaration_specifiers_04(t):
     '''declaration_specifiers : type_specifier declaration_specifiers'''
     t[1].set_base_type(t[2])
     t[1].qualifiers = t[2].qualifiers
     t[1].storageclass = t[2].storageclass
     t[0] = t[1]

def p_declaration_specifiers_05(t):
     '''declaration_specifiers : type_qualifier'''
     myt = Type()
     myt.qualifiers = QualifierList(t[1])
     t[0] = myt

def p_declaration_specifiers_06(t):
     '''declaration_specifiers : type_qualifier declaration_specifiers'''
     t[2].qualifiers.add(t[1])
     t[0] = t[2]    

def p_type_specifier_01(t):
     '''type_specifier : VOID
                      | CHAR
                      | INT
                      | FLOAT
                      | DOUBLE'''
     t[0] = BaseType(t[1])

def p_type_specifier_02(t):
     '''type_specifier : SHORT
                      | LONG
                      | SIGNED
                      | UNSIGNED'''
     t[0] = TypeModifier(t[1])


#----------------LIMEI-------------------------------------------
#def p_type_specifier_03(t):
#    '''type_specifier : struct_or_union_specifier
#                     | enum_specifier'''
#                      # | TYPE_NAME
#    t[0] = t[1]
          
          # FALTA TYPE_NAME


#def p_struct_or_union_specifier(t):
#    '''struct_or_union_specifier : struct_or_union ID LBRACE struct_declaration_list RBRACE
#                                 | struct_or_union LBRACE struct_declaration_list RBRACE
#                                 | struct_or_union ID'''

#def p_struct_or_union(t):
#    '''struct_or_union: STRUCT
#                      | UNION'''

#def p_struct_declaration_list(t):
#     '''struct_declaration_list : struct_declaration
#                               | struct_declaration_list struct_declaration'''

#def p_struct_declaration(t):
#     '''struct_declaration : specifier_qualifier_list struct_declarator_list SEMICOLON'''

#def p_type_name_01(t):
#     '''type_name : specifier_qualifier_list'''

#def p_type_name_02(t):
#     '''type_name : specifier_qualifier_list abstract_declarator'''

#def p_specifier_qualifier_list_01(t):
#     '''specifier_qualifier_list : type_specifier specifier_qualifier_list'''

#def p_specifier_qualifier_list_02(t):
#     '''specifier_qualifier_list : type_specifier'''

#def p_specifier_qualifier_list_03(t):
#     '''specifier_qualifier_list : type_qualifier specifier_qualifier_list'''

#def p_specifier_qualifier_list_04(t):
#     '''specifier_qualifier_list : type_qualifier'''

#def p_struct_declarator_list(t):
#    '''struct_declarator_list : struct_declarator
#                              | struct_declarator_list COMMA struct_declaraistor'''

#def p_struct_declarator(t)
#    '''struct_declarator : declarator
#                         | COLON constant_expression
#                         | declarator COLON constant_expression'''

#def p_enum_specifier(t):
#    '''enum_specifier : ENUM LBRACE enumerator_list RBRACE
#                      | ENUM ID LBRACE enumerator_list RBRACE
#                      | ENUM ID'''

#def p_enumerator_list(t):
#    '''enumerator_list : enumerator
#                       | enumerator_list COMMA enumerator'''

#def p_enumerator(t):
#    '''enumerator : ID
#                  | ID ASSIGN constant_expression'''
#--------------------------------------------------------------------
        
def p_storage_class_specifier_01(t):
     '''storage_class_specifier : TYPEDEF'''
     t[0] = TypedefStorageClass(t[1])

def p_storage_class_specifier_02(t):
     '''storage_class_specifier : EXTERN'''
     t[0] = ExternStorageClass(t[1])

def p_storage_class_specifier_03(t):
     '''storage_class_specifier : STATIC'''
     t[0] = StaticStorageClass(t[1])

def p_storage_class_specifier_04(t):
     '''storage_class_specifier : AUTO'''
     t[0] = AutoStorageClass(t[1])

def p_storage_class_specifier_05(t):
     '''storage_class_specifier : REGISTER'''
     t[0] = RegisterStorageClass(t[1])

def p_storage_class_specifier_06(t):
     '''storage_class_specifier : TREB_PAROUT'''
     t[0] = ParoutStorageClass(t[1])

def p_type_qualifier_01(t):
     '''type_qualifier : CONST'''
     t[0] = ConstQualifier(t[1])

def p_type_qualifier_02(t):
     '''type_qualifier : VOLATILE'''
     t[0] = VolatileQualifier(t[1])

def p_type_qualifier_list_01(t):
     '''type_qualifier_list : type_qualifier'''
     t[0] = QualifierList(t[1])

def p_type_qualifier_list_02(t):
     '''type_qualifier_list : type_qualifier_list type_qualifier'''
     t[1].add(t[2])
     t[0] = t[1]

def p_declarator_01(t):
     '''declarator : direct_declarator'''
     t[0] = t[1]

def p_declarator_02(t):
     '''declarator : pointer declarator'''
     t[2].set_base_type(t[1])
     t[0] = t[2]

def p_pointer_01(t):
     '''pointer : ASTERISK'''
     t[0] = PointerType()

def p_pointer_02(t):
     '''pointer : ASTERISK type_qualifier_list'''
     myt = PointerType()
     myt.qualifiers = t[2]
     t[0] = myt

def p_pointer_03(t):
     '''pointer : ASTERISK pointer'''
     t[2].set_base_type(PointerType())
     t[0] = t[2]


def p_pointer_04(t):
     '''pointer : ASTERISK type_qualifier_list pointer'''
     if isinstance(t[3], QualifierList):
        t[3].add(t[2])
        t[0] = t[3]
     else:
        t[0] = t[2]

def p_declaration_list_02(t):
     '''declaration_list : declaration'''
     t[0] = DeclarationList(t[1])

def p_declaration_list_03(t):
     '''declaration_list : declaration_list declaration'''
     t[1].add(t[2])
     t[0] = t[1]
    
def p_declaration_01(t):
     '''declaration : declaration_specifiers SEMICOLON'''
     t[0] = t[1]

def p_declaration_02(t):
     '''declaration : declaration_specifiers init_declarator_list SEMICOLON'''
     for dec in t[2].nodes:
        dec.set_base_type(t[1])
     t[0] = t[2]

def p_init_declarator_list_01(t):
     '''init_declarator_list : init_declarator'''
     t[0] = DeclarationList(t[1])

def p_init_declarator_list_02(t):
     '''init_declarator_list : init_declarator_list COMMA init_declarator'''
     t[1].add(t[3])
     t[0] = t[1]

def p_init_declarator_01(t):
     '''init_declarator : declarator'''
     t[0] = t[1]

def p_init_declarator_02(t):
     '''init_declarator : declarator ASSIGN initializer'''
     t[1].initialize(t[3], t.lineno(1))
     t[0] = t[1]

def p_initializer_01(t):
     '''initializer : assignment_expression'''
     t[0] = t[1]

def p_initializer_02(t):
     '''initializer : LBRACE initializer_list RBRACE
                    | LBRACE initializer_list COMMA RBRACE'''
     t[0] = t[2]

def p_initializer_list_01(t):
     '''initializer_list : initializer'''
     t[0] = InitializerList(t[1])

def p_initializer_list_02(t):
     '''initializer_list : initializer_list COMMA initializer'''
     t[1].add(t[3])
     t[0] = t[1]

def p_compound_statement_01(t):
     '''compound_statement : LBRACE RBRACE'''
     t[0] = NullNode()

def p_compound_statement_02(t):
     '''compound_statement : LBRACE statement_list RBRACE'''
     t[0] = CompoundStatement(NullNode(), t[2])

def p_compound_statement_03(t):
     '''compound_statement : LBRACE declaration_list RBRACE'''
     t[0] = CompoundStatement(t[2], NullNode())

def p_compound_statement_04(t):
     '''compound_statement : LBRACE declaration_list statement_list RBRACE'''
     t[0] = CompoundStatement(t[2], t[3])

def p_statement_list_01(t):
     '''statement_list : statement'''
     t[0] = StatementList(t[1])

def p_statement_list_02(t):
     '''statement_list : statement_list statement'''
     t[1].add(t[2])
     t[0] = t[1]

def p_statement(t):
     '''statement : labeled_statement
                  | super_statement
                  | compound_statement
                  | expression_statement
                  | selection_statement
                  | iteration_statement
                  | jump_statement'''
     t[0] = t[1]

def p_labeled_statement_01(t):
     '''labeled_statement : CASE constant_expression COLON statement'''
     t[0] = CaseStatement(t[4],t[2])

def p_labeled_statement_02(t):
     '''labeled_statement : DEFAULT COLON statement'''
     t[0] = DefaultStatement(t[3])

#---------------------------LIMEI--------------------
#def p_labeled_statement_03(t):
#    '''labeled_statement : ID COLON statement'''
#----------------------------------------------------

def p_expression_statement_01(t):
     '''expression_statement : SEMICOLON'''
     #t[0] = NullNode()
     pass

def p_expression_statement_02(t):
     '''expression_statement : expression SEMICOLON'''
     t[0] = t[1]

def p_selection_statement_01(t):
     '''selection_statement : IF LPAREN expression RPAREN statement'''
     t[0] = IfStatement(t[3], t[5])

def p_selection_statement_02(t):
     '''selection_statement : IF LPAREN expression RPAREN statement ELSE statement'''
     t[0] = IfStatement(t[3], t[5], t[7])

def p_selection_statement_03(t):
     '''selection_statement : SWITCH LPAREN expression RPAREN statement'''
     t[0] = SwitchStatement(t[3], t[5])

def p_jump_statement_01(t):
     '''jump_statement : RETURN SEMICOLON'''
     t[0] = ReturnStatement(NullNode())
    
def p_jump_statement_02(t):
     '''jump_statement : RETURN expression SEMICOLON'''
     t[0] = ReturnStatement(t[2])

def p_jump_statement_03(t):
     '''jump_statement : BREAK SEMICOLON'''
     t[0] = BreakStatement()

def p_jump_statement_04(t):
     '''jump_statement : CONTINUE SEMICOLON'''
     t[0] = ContinueStatement()

#---------------------------LIMEI--------------------
#def p_jump_statement_05(t):
#    '''jump_statement : GOTO ID SEMICOLON'''
#----------------------------------------------------

def p_iteration_statement_01(t):
     '''iteration_statement : WHILE LPAREN expression RPAREN statement'''
     t[0] = WhileLoop(t[3], t[5])

def p_iteration_statement_02(t):
     '''iteration_statement : FOR LPAREN expression_statement expression_statement expression RPAREN statement'''
     t[0] = ForLoop(t[3], t[4], t[5], t[7])

def p_iteration_statement_03(t):
     '''iteration_statement : FOR LPAREN expression_statement expression_statement RPAREN statement'''
     t[0] = ForLoop(t[3], t[4], NullNode(), t[6])

def p_iteration_statement_04(t):
     '''iteration_statement : DO statement WHILE LPAREN expression RPAREN SEMICOLON'''
     t[0] = DoWhileLoop(t[5], t[2])

def p_expression_01(t):
     '''expression : assignment_expression'''
     t[0] = t[1]

def p_expression_02(t):
     '''expression : expression COMMA assignment_expression'''
     t[1].add(t[3])
     t[0] = t[1]

def p_assignment_expression_01(t):
     '''assignment_expression : conditional_expression'''
     t[0] = t[1]

def p_assignment_expression_02(t):
     '''assignment_expression : unary_expression ASSIGN assignment_expression
                              | unary_expression EQ_PLUS assignment_expression
                              | unary_expression EQ_MINUS assignment_expression
                              | unary_expression EQ_TIMES assignment_expression
                              | unary_expression EQ_DIV assignment_expression
                              | unary_expression EQ_MODULO assignment_expression
                              | unary_expression EQ_SHIFT_LEFT assignment_expression
                              | unary_expression EQ_SHIFT_RIGHT assignment_expression
                              | unary_expression EQ_AMPERSAND assignment_expression
                              | unary_expression EQ_CARET assignment_expression
                              | unary_expression EQ_PIPE assignment_expression'''                        
     t[0] = Binop(t[1], t[3], t[2])

def p_constant_expression_01(t):
     '''constant_expression : conditional_expression'''
     t[0] = t[1]


def p_conditional_expression_01(t):
     '''conditional_expression : logical_or_expression'''
     t[0] = t[1]

#------------- LIMEI ------------------------------------------
#def p_conditional_expression_02(t):
#     '''conditional_expression : logical_or_expression QUESTION expression COLON conditional_expression'''
#---------------------------------------------------------------


def p_logical_or_expression_01(t):
     '''logical_or_expression : logical_and_expression'''
     t[0] = t[1]

def p_logical_or_expression_02(t):
     '''logical_or_expression : logical_or_expression DOUBLE_PIPE logical_and_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_logical_and_expression_01(t):
     '''logical_and_expression : inclusive_or_expression'''
     t[0] = t[1]

def p_logical_and_expression_02(t):
     '''logical_and_expression : logical_and_expression DOUBLE_AMPERSAND inclusive_or_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_inclusive_or_expression_01(t):
     '''inclusive_or_expression : exclusive_or_expression'''
     t[0] = t[1]

def p_inclusive_or_expression_02(t):
     '''inclusive_or_expression : inclusive_or_expression PIPE exclusive_or_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_exclusive_or_expression_01(t):
     '''exclusive_or_expression : and_expression'''
     t[0] = t[1]

def p_exclusive_or_expression_02(t):
     '''exclusive_or_expression : exclusive_or_expression CARET and_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_and_expression_01(t):
     '''and_expression : equality_expression'''
     t[0] = t[1]

def p_and_expression_02(t):
     '''and_expression : and_expression AMPERSAND equality_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_equality_expression_01(t):
     '''equality_expression : relational_expression'''
     t[0] = t[1]

def p_equality_expression_02(t):    
     '''equality_expression : equality_expression EQ relational_expression
                            | equality_expression NOT_EQ relational_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_relational_expression_01(t):
     '''relational_expression : shift_expression'''
     t[0] = t[1]

def p_relational_expression_02(t):
     '''relational_expression : relational_expression LESS shift_expression
                              | relational_expression GREATER shift_expression
                              | relational_expression LESS_EQ shift_expression
                              | relational_expression GREATER_EQ shift_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_shift_expression_01(t):
     '''shift_expression : additive_expression'''
     t[0] = t[1]

def p_shift_expression_02(t):
     '''shift_expression : shift_expression SHIFT_LEFT additive_expression
                         | shift_expression SHIFT_RIGHT additive_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_additive_expression_01(t):
     '''additive_expression : mult_expression'''
     t[0] = t[1]

def p_additive_expression_02(t):
     '''additive_expression : additive_expression PLUS mult_expression
                           | additive_expression MINUS mult_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_mult_expression_01(t):
     '''mult_expression : cast_expression'''
     t[0] = t[1]

def p_mult_expression_02(t):
     '''mult_expression : mult_expression ASTERISK cast_expression
                       | mult_expression DIV cast_expression    
                       | mult_expression MODULO cast_expression'''
     t[0] = _get_calculated(Binop(t[1], t[3], t[2]))

def p_cast_expression(t):
     '''cast_expression : unary_expression'''
     t[0] = t[1]

#------------ LIMEI ----------------------------------------------------
#def p_cast_expression(t):
#     '''cast_expression : LPAREN type_name RPAREN cast_expression'''
#----------------------------------------------------------------------

def p_unary_expression_01(t):
     '''unary_expression : postfix_expression'''
     t[0] = t[1]

def p_unary_expression_02(t):
     '''unary_expression        : DOUBLE_PLUS unary_expression'''
     t[0] = _get_calculated(Binop(t[2], Const(1, BaseType('int')), '+'))

def p_unary_expression_03(t):
     '''unary_expression        : DOUBLE_MINUS unary_expression'''
     t[0] = _get_calculated(Binop(t[2], Const(1, BaseType('int')), '-'))

def p_unary_expression_04(t):
     '''unary_expression        : AMPERSAND cast_expression'''
     t[0] = AddrOf(t[2])

def p_unary_expression_05(t):
     '''unary_expression        : ASTERISK cast_expression'''
     t[0] = Pointer(t[2])

def p_unary_expression_06(t):
     '''unary_expression        : PLUS cast_expression'''
     t[0] = t[2]

def p_unary_expression_07(t):
     '''unary_expression        : MINUS cast_expression'''
     t[0] = _get_calculated(Negative(t[2]))

def p_unary_expression_08(t):
     '''unary_expression        : TILDE cast_expression'''
     t[0] = _get_calculated(BinNot(t[2]))

def p_unary_expression_09(t):
     '''unary_expression        : EXCLAMATION cast_expression'''
    # horrible hack for the '!' operator... Just insert an
    # (expr == 0) into the AST.
     t[0] = _get_calculated(Binop(t[2], Const(0, BaseType('int')), '=='))


#----------------LIMEI---------------------------------------------------
#def p_unary_expression_10(t):
#    '''unary_expression        : SIZEOF unary_expression'''

#def p_unary_expression_11(t):
#    '''unary_expression        : SIZEOF LPAREN type_name RPAREN'''
#------------------------------------------------------------------------

def p_postfix_expression_01(t):
     '''postfix_expression : primary_expression'''
     t[0] = t[1]

def p_postfix_expression_02(t):
     '''postfix_expression : postfix_expression LPAREN argument_expression_list RPAREN'''
     t[0] = FunctionExpression(t[1], t[3])
     pass

def p_postfix_expression_03(t):
     '''postfix_expression : postfix_expression LPAREN RPAREN'''
     t[0] = FunctionExpression(t[1], ArgumentList())

def p_postfix_expression_04(t):
     '''postfix_expression : postfix_expression LBRACKET expression RBRACKET'''
     t[0] = ArrayExpression(t[1], t[3])

def p_postfix_expression_05(t):
     '''postfix_expression : postfix_expression DOUBLE_PLUS'''
     t[0] = _get_calculated(Binop(t[1], Const(1, BaseType('int')), '+'))

def p_postfix_expression_06(t):
     '''postfix_expression : postfix_expression DOUBLE_MINUS'''
     t[0] = _get_calculated(Binop(t[1], Const(1, BaseType('int')), '-'))


# --------------- LIMEI --------------------------------------------
#def p_postfix_expression_07(t):
#     '''postfix_expression : postfix_expression t_ARROW ID'''

#def p_postfix_expression_08(t):
#    '''postfix_expression : postfix_expression DOT ID'''
#-----------------------------------------------------------------------

def p_primary_expression_01(t):
     '''primary_expression : ID'''
     t[0] = Id(t[1], t.lineno(1))

#CONSTANT
def p_primary_expression_02(t):
     '''primary_expression : INUMBER'''
     t[0] = Const(int(t[1]), BaseType('int'))

def p_primary_expression_03(t):
     '''primary_expression : FNUMBER'''
     print("float!!!")
     t[0] = Const(float(t[1]), BaseType('float'))

def p_primary_expression_04(t):
     '''primary_expression : CHARACTER'''
     t[0] = Const(ord(eval(t[1])), BaseType('char'))

def p_primary_expression_05(t):
     '''primary_expression : string_literal'''
     t[0] = t[1]

def p_primary_expression_06(t):
     '''primary_expression : LPAREN expression RPAREN'''
     t[0] = t[2]

def p_string_literal_01(t):
     '''string_literal : STRING'''
     t[0] = StringLiteral(eval(t[1]))

def p_string_literal_02(t):
     '''string_literal : string_literal STRING'''
     t[1].append_str(eval(t[2]))
     t[0] = t[1]

def p_direct_declarator_01(t):
     '''direct_declarator : ID'''
     t[0] = Declaration(t[1])

def p_direct_declarator_02(t):
     '''direct_declarator : direct_declarator LPAREN parameter_type_list RPAREN'''
     t[1].add_type(FunctionType(t[3]))
     t[0] = t[1]

def p_direct_declarator_03(t):
     '''direct_declarator : direct_declarator LPAREN RPAREN'''
     t[1].add_type(FunctionType(ParamList()))
     t[0] = t[1]

def p_direct_declarator_04(t):
     '''direct_declarator : LPAREN declarator RPAREN'''
     t[0] = t[2]

def p_direct_declarator_05(t):
     '''direct_declarator : direct_declarator LBRACKET constant_expression RBRACKET'''
     t[1].add_type(ArrayType(t[3]))
     t[0] = t[1]        

def p_direct_declarator_06(t):
     '''direct_declarator : direct_declarator LBRACKET RBRACKET'''
     t[1].add_type(ArrayType(Const(0, BaseType('int'))))
     t[0] = t[1]


#-----------------------LIMEI PQ EH OLD STYLE---------------------------------
#double alt_style( a , real )  /* Obsolete function definition */
#    double *real; 
#    int a; 
#{
#    return ( *real + a ) ;
#}
#def p_direct_declarator_07(t):
#     '''direct_declarator : direct_declarator LPAREN identifier_list RPAREN'''


#def p_identifier_list_01(t):
#     '''identifier_list : ID'''
#     t[0] = Id(t[1], t.lineno(1))

#def p_identifier_list_02(t):
#     '''identifier_list : identifier_list COMMA ID'''
#     t[1].add(Id(t[1], t.lineno(1)))
#     t[0]=t[1]
#--------------------------------------------------------------------------------
def p_parameter_type_list_01(t):
     '''parameter_type_list : parameter_list'''
     t[0] = t[1]

def p_parameter_type_list_02(t):
     '''parameter_type_list : parameter_list COMMA ELLIPSIS'''
     t[1].has_ellipsis = 1
     t[0] = t[1]

def p_parameter_list_01(t):
     '''parameter_list : parameter_declaration'''
     t[0] = ParamList(t[1])

def p_parameter_list_02(t):
     '''parameter_list : parameter_list COMMA parameter_declaration'''
     t[1].add(t[3])
     t[0] = t[1]

def p_parameter_declaration_01(t):
     '''parameter_declaration : declaration_specifiers declarator'''
     t[2].set_base_type(t[1])
     t[0] = t[2]

#Nao sei pq essa regra.... sem declarator eh bizonho
def p_parameter_declaration_02(t):
     '''parameter_declaration : declaration_specifiers'''
     t[0] = t[1]

#------------------------LIMEI--------------------------------------------------------------------
#def p_parameter_declaration_03(t):
#     '''parameter_declaration : declaration_specifiers abstract_declarator'''
#
#def p_abstract_declarator(t):
#     '''abstract_declarator : pointer
#                            | direct_abstract_declarator
#                            | pointer direct_abstract_declarator'''
#
#def p_direct_abstract_declarator(t):
#     '''direct_abstract_declarator : LPAREN abstract_declarator RPAREN
#                                   | LBRACKET RBRACKET
#                                   | LBRACKET constant_expression RBRACKET
#                                   | direct_abstract_declarator LBRACKET RBRACKET
#                                   | direct_abstract_declarator LBRACKET constant_expression RBRACKET
#                                   | LPAREN RPAREN
#                                   | LPAREN parameter_type_list RPAREN
#                                   | direct_abstract_declarator LPAREN RPAREN
#                                   | direct_abstract_declarator LPAREN parameter_type_list RPAREN'''
#-------------------------------------------------------------------------------------------------------------

def p_argument_expression_list_01(t):
     '''argument_expression_list : assignment_expression'''
     t[0] = ArgumentList(t[1])

def p_argument_expression_list_02(t):
     '''argument_expression_list : argument_expression_list COMMA assignment_expression'''
     t[1].add(t[3])
     t[0] = t[1]


def p_super_statement_01(t):
    '''super_statement : TREB_SUPER SINGLE super_input_statement super_output_statement SUPERBODY'''
    t[0] = SuperInstruction(t[5], t[3], t[4])


def p_super_statement_02(t):
    '''super_statement : TREB_SUPER PARALLEL super_input_statement super_output_statement SUPERBODY'''
    t[0] = ParInstruction(t[5], t[3], t[4])


def p_super_statement_03(t):
    '''super_statement : TREB_SUPER REDUCE INPUT LPAREN super_input_list RPAREN OUTPUT LPAREN super_output_list RPAREN SUPERBODY'''
    t[0] = ReduceInstruction(t[11], t[5], t[9])

def p_super_output_statement_01(t):
        '''super_output_statement : OUTPUT LPAREN super_output_list RPAREN'''
        t[0] = t[3]

def p_super_output_statement_02(t):
        '''super_output_statement : '''
        t[0] = ArgumentList(None)

def p_super_output_list_01(t):
    '''super_output_list : ID'''
    t[0] = ArgumentList(Id(t[1], t.lineno(1)))

def p_super_output_list_02(t):
    '''super_output_list : super_output_list COMMA ID'''
    t[1].add(Id(t[3], t.lineno(1)))
    t[0] = t[1]

def p_super_input_statement_01(t):
        '''super_input_statement : INPUT LPAREN super_input_list RPAREN'''
        t[0] = t[3]

def p_super_input_statement_02(t):
        '''super_input_statement : '''
        t[0] = ArgumentList(None)

def p_super_input_list_01(t):
    '''super_input_list : parout_expression'''
    t[0] = ArgumentList(t[1])

def p_super_input_list_02(t):
    '''super_input_list : super_input_list COMMA parout_expression'''
    t[1].add(t[3])
    t[0] = t[1]

def p_parout_expression_01(t):
    '''parout_expression : super_input_id COLON COLON INUMBER '''
    t[1].set_index(int(t[4]))
    t[0] = t[1]

def p_parout_expression_02(t):
    '''parout_expression : super_input_id COLON COLON ASTERISK'''
    t[1].set_all()
    t[0] = t[1]

def p_parout_expression_03(t):
    '''parout_expression : super_input_id COLON COLON MYTID '''
    t[1].set_mytid()
    t[0] = t[1]

def p_parout_expression_04(t):
    '''parout_expression : super_input_id'''
    t[0] = t[1]

def p_parout_expression_05(t):
    '''parout_expression : super_input_id COLON COLON LPAREN MYTID mytid_op INUMBER RPAREN'''
    t[1].set_mytid_expr(t[6], int(t[7]))
    t[0] = t[1]
    
def p_parout_expression_06(t):
    '''parout_expression : super_input_id COLON COLON LASTTID '''
    t[1].set_last()
    t[0] = t[1]

def p_mytid_op_01(t):
    '''mytid_op : PLUS
                | MINUS'''
    t[0] = t[1]

def p_super_input_id_01(t):
    '''super_input_id : ID'''
    t[0] = Id(t[1], t.lineno(1))

def p_super_input_id_02(t):
    '''super_input_id : LOCAL DOT ID'''
    t[0] = Id(t[3], t.lineno(1), local=True)
    
def p_super_input_id_03(t):
    '''super_input_id : STARTER DOT ID'''
    t[0] = Id(t[3], t.lineno(1), starter=True)

def p_block_statement_01(t):
    '''block_statement : BLOCK'''
    t[0] = Block(t[1])

# -----------

#def p_empty(t):
#    'empty :'
#    pass

def p_error(t):
    # print "You've got a syntax error somewhere in your code."
    # print "It could be around line %d." % t.lineno
    print("Error in line %d." % t.lineno)
     #print "Good luck finding it."
    raise ParseError()

yacc.yacc(debug=1)

#  ---------------------------------------------------------------
#  End of cparse.py
#  ---------------------------------------------------------------
