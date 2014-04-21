/*
    Antlr v4 Grammar for XPath 3.0
    Copyright (C) 2014 Adam Retter

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
 
/**
 * Antlr v4 Grammar for XPath 3.0
 * as defined by the W3C
 * http://www.w3.org/TR/xpath-30/
 * 
 * @author Adam Retter <adam.retter@googlemail.com>
 */
grammar XPath3;

import XMLNames11;


/** [1] XPath ::= Expr */
xpath : expr ;

/** [2] ParamList ::= Param ("," Param)* */
paramList : param (COMMA_CHAR param)* ;    
    
/** [3] Param ::= "$" EQName TypeDeclaration? */
param : DOLLAR_SIGN_CHAR eQName typeDeclaration? ;

/** [4] FunctionBody ::= EnclosedExpr */
functionBody : enclosedExpr ;

/** [5] EnclosedExpr ::= "{" Expr "}" */
enclosedExpr : LEFT_CURLY_BRACKET_CHAR expr RIGHT_CURLY_BRACKET_CHAR ;

/** [6] Expr ::= ExprSingle ("," ExprSingle)* */
expr : exprSingle (COMMA_CHAR exprSingle)* ;

/**
 * [7] ExprSingle ::= ForExpr
 * | LetExpr
 * | QuantifiedExpr
 * | IfExpr
 * | OrExpr
 */
exprSingle : forExpr
           | letExpr
           | quantifiedExpr
           | ifExpr
           | orExpr ;

/** [8] ForExpr ::= SimpleForClause "return" ExprSingle */
forExpr : simpleForClause RETURN exprSingle ;

/** [9] SimpleForClause ::= "for" SimpleForBinding ("," SimpleForBinding)* */
simpleForClause : FOR simpleForBinding (COMMA_CHAR simpleForBinding)* ;

/** [10] SimpleForBinding ::= "$" VarName "in" ExprSingle */
simpleForBinding : DOLLAR_SIGN_CHAR varName IN exprSingle ;

/** [11] LetExpr ::= SimpleLetClause "return" ExprSingle */
letExpr : simpleLetClause RETURN exprSingle ;

/** [12] SimpleLetClause ::= "let" SimpleLetBinding ("," SimpleLetBinding)* */
simpleLetClause : LET simpleLetBinding (COMMA_CHAR simpleLetBinding)* ;

/** [13] SimpleLetBinding ::= "$" VarName ":=" ExprSingle */
simpleLetBinding : DOLLAR_SIGN_CHAR varName ':=' exprSingle ;

/** [14] QuantifiedExpr ::= ("some" | "every") "$" VarName "in" ExprSingle ("," "$" VarName "in" ExprSingle)* "satisfies" ExprSingle **/
quantifiedExpr : (SOME | EVERY) DOLLAR_SIGN_CHAR varName IN exprSingle (COMMA_CHAR DOLLAR_SIGN_CHAR varName IN exprSingle)* SATISFIES exprSingle ;

/** [15] IfExpr ::= "if" "(" Expr ")" "then" ExprSingle "else" ExprSingle */
ifExpr: IF LEFT_PARENTHESIS_CHAR expr RIGHT_PARENTHESIS_CHAR THEN exprSingle ELSE exprSingle ;

/** [16] OrExpr ::= AndExpr ( "or" AndExpr )* */
orExpr : andExpr (OR andExpr)* ;
    
/** [17] AndExpr ::= ComparisonExpr ( "and" ComparisonExpr )* */
andExpr : comparisonExpr (AND comparisonExpr)* ;
    
/** [18] ComparisonExpr ::= StringConcatExpr ( (ValueComp
 *  | GeneralComp
 *  | NodeComp) StringConcatExpr )?
 */
comparisonExpr  : stringConcatExpr (
                    (valueComp | generalComp | nodeComp)
                stringConcatExpr)? ;

/** [19] StringConcatExpr ::= RangeExpr ( "||" RangeExpr )* */
stringConcatExpr : rangeExpr (OR_SYMBOL rangeExpr)* ;

/** [20] RangeExpr ::= AdditiveExpr ( "to" AdditiveExpr )? */
rangeExpr : additiveExpr (TO additiveExpr)? ;

/** [21] AdditiveExpr ::= MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )* */
additiveExpr : multiplicativeExpr ((PLUS_SIGN_CHAR | HYPHEN_MINUS_CHAR) multiplicativeExpr)* ;

/** [22] MultiplicativeExpr ::= UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )* */
multiplicativeExpr : unionExpr ((ASTERISK_CHAR | DIV | IDIV | MOD) unionExpr)* ;
    
/** [23] unionExpr ::= IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )* */
unionExpr : intersectExceptExpr ((UNION | VERTICAL_LINE_CHAR) intersectExceptExpr)* ;

/** [24] IntersectExceptExpr ::= InstanceofExpr ( ("intersect" | "except") InstanceofExpr )* */
intersectExceptExpr : instanceofExpr ((INTERSECT | EXCEPT) instanceofExpr )* ;
    
/** [25] InstanceofExpr ::= TreatExpr ( "instance" "of" SequenceType )? */
instanceofExpr : treatExpr (INSTANCE OF sequenceType)? ;
    
/** [26] TreatExpr ::= CastableExpr ( "treat" "as" SequenceType )? */
treatExpr : castableExpr (TREAT AS sequenceType )? ;

/** [27] CastableExpr ::= CastExpr ( "castable" "as" SingleType )? */
castableExpr : castExpr (CASTABLE AS singleType)? ;
    
/** [28] CastExpr ::= UnaryExpr ( "cast" "as" SingleType )? */
castExpr : unaryExpr (CAST AS singleType)? ;

/** [29] UnaryExpr ::= ("-" | "+")* ValueExpr */
unaryExpr : (HYPHEN_MINUS_CHAR | PLUS_SIGN_CHAR)* valueExpr ;

/** [30] ValueExpr ::= SimpleMapExpr */
valueExpr : simpleMapExpr ;

/** [31] GeneralComp ::= "=" | "!=" | "<" | "<=" | ">" | ">=" */
generalComp : EQUALS_SYMBOL | NOT_EQUALS_SYMBOL | LESS_THAN_SYMBOL | LESS_THAN_OR_EQUALS_SYMBOL | GREATER_THAN_SYMBOL | GREATER_THAN_OR_EQUALS_SYMBOL ;

/** [32] ValueComp ::= "eq" | "ne" | "lt" | "le" | "gt" | "ge" */
valueComp : EQ | NE | LT | LE | GT | GE ;

/** [33] NodeComp ::= "is" | "<<" | ">>" */
nodeComp : IS | PRECEDES_SYMBOL | FOLLOWS_SYMBOL ;

/** [34] SimpleMapExpr ::= PathExpr ("!" PathExpr)* */
simpleMapExpr : pathExpr (EXCLAMATION_MARK_CHAR pathExpr)* ;

/** [35] PathExpr ::= ("/" RelativePathExpr?)
 *  | ("//" RelativePathExpr)
 *  | RelativePathExpr
 * 
 * //xgc: leading-lone-slash
 */
pathExpr : (CHILDREN_SYMBOL relativePathExpr?)
           | (DESCENDANTS_SYMBOL relativePathExpr)
           | relativePathExpr ;
        
/** [36] RelativePathExpr ::= StepExpr (("/" | "//") StepExpr)* */
relativePathExpr : stepExpr ((CHILDREN_SYMBOL | DESCENDANTS_SYMBOL) stepExpr)* ;

/** [37] StepExpr ::= PostfixExpr | AxisStep */
stepExpr : postfixExpr | axisStep ;                   
                   
/** [38] AxisStep ::=   	(ReverseStep | ForwardStep) PredicateList */
axisStep : (reverseStep | forwardStep) predicateList ;

/** [39] ForwardStep ::= (ForwardAxis NodeTest) | AbbrevForwardStep */
forwardStep : (forwardAxis nodeTest) | abbrevForwardStep ;

/**
 * [40] ForwardAxis ::= ("child" "::")
 * | ("descendant" "::")
 * | ("attribute" "::")
 * | ("self" "::")
 * | ("descendant-or-self" "::")
 * | ("following-sibling" "::")
 * | ("following" "::")
 * | ("namespace" "::")
 */
forwardAxis :   (
                    CHILD
                    | DESCENDANT
                    | ATTRIBUTE
                    | SELF
                    | DESCENDANT_OR_SELF
                    | FOLLOWING_SIBLING
                    | FOLLOWING
                    | NAMESPACE
                ) AXIS_NODE_SYMBOL ;

/** [41] AbbrevForwardStep ::= "@"? NodeTest */
abbrevForwardStep : ATTRIBUTE_SYMBOL? nodeTest ;

/** [42] ReverseStep ::= (ReverseAxis NodeTest) | AbbrevReverseStep */
reverseStep : (reverseAxis nodeTest) | abbrevReverseStep ;

/** [43] ReverseAxis ::= ("parent" "::")
 *  | ("ancestor" "::")
 *  | ("preceding-sibling" "::")
 *  | ("preceding" "::")
 *  | ("ancestor-or-self" "::")
 */
reverseAxis :   (
                    PARENT
                    | ANCESTOR
                    | PRECEDING_SIBLING
                    | PRECEDING
                    | ANCESTOR_OR_SELF
                ) AXIS_NODE_SYMBOL ;


/** [44] AbbrevReverseStep ::= ".." */
abbrevReverseStep : REVERSE_SYMBOL ;
                      
/** [45] NodeTest ::= KindTest | NameTest */
nodeTest : kindTest | nameTest ;
    
/** [46] NameTest ::= EQName | Wildcard */
nameTest : eQName | wildcard ;
    
/** [47] Wildcard ::= "*"
 *  | (NCName ":" "*")
 *  | ("*" ":" NCName)
 *  | (BracedURILiteral "*")
 *
 * //ws: explicit
 */
wildcard :  ASTERISK_CHAR
            | (nCName COLON_CHAR ASTERISK_CHAR)
            | (ASTERISK_CHAR COLON_CHAR nCName)
            | (bracedURILiteral ASTERISK_CHAR) ;
            
/** [48] PostfixExpr ::= PrimaryExpr (Predicate | ArgumentList)* */
postfixExpr : primaryExpr (predicate | argumentList)* ;

/** [49] ArgumentList ::= "(" (Argument ("," Argument)*)? ")" */
argumentList : LEFT_PARENTHESIS_CHAR (argument (COMMA_CHAR argument)*)? RIGHT_PARENTHESIS_CHAR ;
    
/** [50] PredicateList ::= Predicate* */
predicateList : predicate* ;
    
/** [51] Predicate ::= "[" Expr "]" */
predicate : LEFT_SQUARE_BRACKET_CHAR expr RIGHT_SQUARE_BRACKET_CHAR ;
    
/** [52] PrimaryExpr ::= Literal
 *  | VarRef
 *  | ParenthesizedExpr
 *  | ContextItemExpr
 *  | FunctionCall
 *  | FunctionItemExpr
 */
primaryExpr :   literal
                | varRef
                | parenthesizedExpr
                | contextItemExpr
                | functionCall
                | functionItemExpr ;

    
/** [53] Literal ::= NumericLiteral | StringLiteral */
literal : numericLiteral | stringLiteral ;
    
/** [54] NumericLiteral ::= IntegerLiteral | DecimalLiteral | DoubleLiteral **/
numericLiteral : integerLiteral | decimalLiteral | doubleLiteral ;
    
/** [55] VarRef ::= "$" VarName */
varRef : DOLLAR_SIGN_CHAR varName ;

/** [56] VarName ::= EQName */
varName : eQName ;

/** [57] ParenthesizedExpr ::= "(" Expr? ")" */
parenthesizedExpr : LEFT_PARENTHESIS_CHAR expr? RIGHT_PARENTHESIS_CHAR ;
    
/** [58] ContextItemExpr ::= "." */
contextItemExpr : FULL_STOP_CHAR ;

/** [59] FunctionCall ::= EQName ArgumentList
 *
 * //xgc: reserved-function-names
 * //gn: parens
 */
functionCall : eQName argumentList ;

/** [60] Argument ::= ExprSingle | ArgumentPlaceholder */
argument : exprSingle | argumentPlaceholder ;
    
/** [61] ArgumentPlaceholder ::= "?" */
argumentPlaceholder : QUESTION_MARK_CHAR ; 
    
/** [62] FunctionItemExpr ::= NamedFunctionRef | InlineFunctionExpr */
functionItemExpr : namedFunctionRef | inlineFunctionExpr ;
    
/** [63] NamedFunctionRef ::= EQName "#" IntegerLiteral
 *
 * //xgc: reserved-function-names
 */
namedFunctionRef : eQName NUMBER_SIGN_CHAR integerLiteral ;
    
/** [64] InlineFunctionExpr ::= "function" "(" ParamList? ")" ("as" SequenceType)? FunctionBody */
inlineFunctionExpr : FUNCTION LEFT_PARENTHESIS_CHAR paramList? RIGHT_PARENTHESIS_CHAR (AS sequenceType)? functionBody ;
    
/** [65] SingleType ::= SimpleTypeName "?"? */
singleType : simpleTypeName QUESTION_MARK_CHAR? ;

/** [66] TypeDeclaration ::= "as" SequenceType */
typeDeclaration : AS sequenceType ;    
    
/** [67] SequenceType ::=  ("empty-sequence" "(" ")")
 *  | (ItemType OccurrenceIndicator?)
 */
sequenceType :  (EMPTY_SEQUENCE LEFT_PARENTHESIS_CHAR RIGHT_PARENTHESIS_CHAR)
                | (itemType occurrenceIndicator?) ;
    
/** [68] OccurrenceIndicator ::= "?" | "*" | "+"
 *
 * //xgc: occurrence-indicators
 */
occurrenceIndicator : QUESTION_MARK_CHAR | ASTERISK_CHAR | PLUS_SIGN_CHAR ;

/** [69] ItemType ::= KindTest | ("item" "(" ")") | FunctionTest | AtomicOrUnionType | ParenthesizedItemType */
itemType : kindTest | (ITEM LEFT_PARENTHESIS_CHAR RIGHT_PARENTHESIS_CHAR) | functionTest | atomicOrUnionType | parenthesizedItemType ;

/** [70] AtomicOrUnionType ::= EQName */
atomicOrUnionType : eQName;


/** [71] KindTest ::= DocumentTest
 *  | ElementTest
 *  | AttributeTest
 *  | SchemaElementTest
 *  | SchemaAttributeTest
 *  | PITest
 *  | CommentTest
 *  | TextTest
 *  | NamespaceNodeTest
 *  | AnyKindTest
 **/
kindTest :  documentTest
            | elementTest
            | attributeTest
            | schemaElementTest
            | schemaAttributeTest
            | pITest
            | commentTest
            | textTest
            | namespaceNodeTest
            | anyKindTest ;
    
/** [72] AnyKindTest ::= "node" "(" ")" */
anyKindTest : NODE LEFT_PARENTHESIS_CHAR RIGHT_PARENTHESIS_CHAR ;

/** [73] DocumentTest ::= "document-node" "(" (ElementTest | SchemaElementTest)? ")" */
documentTest : DOCUMENT_NODE LEFT_PARENTHESIS_CHAR (elementTest | schemaElementTest)? RIGHT_PARENTHESIS_CHAR ;

/** [74] TextTest ::= "text" "(" ")" */
textTest : TEXT LEFT_PARENTHESIS_CHAR RIGHT_PARENTHESIS_CHAR ;
    
/** [75] CommentTest ::= "comment" "(" ")" */
commentTest : COMMENT LEFT_PARENTHESIS_CHAR RIGHT_PARENTHESIS_CHAR ;
    
/** [76] NamespaceNodeTest ::= "namespace-node" "(" ")" */
namespaceNodeTest : NAMESPACE_NODE LEFT_PARENTHESIS_CHAR RIGHT_PARENTHESIS_CHAR ;    
    
/** [77] PITest ::= "processing-instruction" "(" (NCName | StringLiteral)? ")" */
pITest : PROCESSING_INSTRUCTION LEFT_PARENTHESIS_CHAR (nCName | stringLiteral)? RIGHT_PARENTHESIS_CHAR ;
    
/** [78] AttributeTest ::= "attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")" */
attributeTest : ATTRIBUTE LEFT_PARENTHESIS_CHAR (attribNameOrWildcard (COMMA_CHAR typeName)?)? RIGHT_PARENTHESIS_CHAR ;     
    
/** [79] AttribNameOrWildcard ::= AttributeName | "*" */
attribNameOrWildcard : attributeName | ASTERISK_CHAR ;    
    
/** [80] SchemaAttributeTest ::= "schema-attribute" "(" AttributeDeclaration ")" */
schemaAttributeTest : SCHEMA_ATTRIBUTE LEFT_PARENTHESIS_CHAR attributeDeclaration RIGHT_PARENTHESIS_CHAR ;

/** [81] attributeDeclaration ::= attributeName */
attributeDeclaration : attributeName ;

/** [82] ElementTest ::= "element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")" */
elementTest : ELEMENT LEFT_PARENTHESIS_CHAR (elementNameOrWildcard (COMMA_CHAR typeName QUESTION_MARK_CHAR?)?)? RIGHT_PARENTHESIS_CHAR ;

/** [83] ElementNameOrWildcard ::= ElementName | "*" */
elementNameOrWildcard : elementName | ASTERISK_CHAR ;

/** [84] SchemaElementTest ::= "schema-element" "(" ElementDeclaration ")" */
schemaElementTest : SCHEMA_ELEMENT LEFT_PARENTHESIS_CHAR elementDeclaration RIGHT_PARENTHESIS_CHAR ;
    
/** [85] ElementDeclaration ::= ElementName */
elementDeclaration : elementName ;    
    
/** [86] AttributeName ::= EQName */
attributeName : eQName ;
    
/** [87] ElementName ::= EQName */
elementName : eQName ;

/** [88] SimpleTypeName ::= TypeName */
simpleTypeName : typeName ;

/** [89] TypeName ::= EQName */
typeName : eQName ;    
    
/** [90] FunctionTest ::= AnyFunctionTest
 *  | TypedFunctionTest
 */
functionTest : anyFunctionTest | typedFunctionTest ;

/** [91] AnyFunctionTest ::= "function" "(" "*" ")" */
anyFunctionTest : FUNCTION LEFT_PARENTHESIS_CHAR ASTERISK_CHAR RIGHT_PARENTHESIS_CHAR ;
    
/** [92] TypedFunctionTest ::= "function" "(" (SequenceType ("," SequenceType)*)? ")" "as" SequenceType */
typedFunctionTest : FUNCTION LEFT_PARENTHESIS_CHAR (sequenceType (COMMA_CHAR sequenceType)*)? RIGHT_PARENTHESIS_CHAR AS sequenceType ;
    
/** [93] ParenthesizedItemType ::= "(" ItemType ")" */
parenthesizedItemType : LEFT_PARENTHESIS_CHAR itemType RIGHT_PARENTHESIS_CHAR ;

/** [94] EQName ::= QName | URIQualifiedName */
eQName : qName | uRIQualifiedName ;



// TERMINAL SYMBOLS

/** [95] IntegerLiteral ::= Digits */
integerLiteral : Digits ;

/** [96] DecimalLiteral ::= ("." Digits) | (Digits "." [0-9]*)
 *
 * //ws: explicit
 */
decimalLiteral : (FULL_STOP_CHAR Digits) | (Digits FULL_STOP_CHAR Digits?) ;
    
/** [97] DoubleLiteral ::= (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
 *
 * //ws: explicit
 */
doubleLiteral : ((FULL_STOP_CHAR Digits) | (Digits (FULL_STOP_CHAR Digits?)?)) ('e' | 'E') (PLUS_SIGN_CHAR | HYPHEN_MINUS_CHAR)? Digits ;
    
/** [98] StringLiteral ::= ('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
 *
 * //ws: explicit
 */
stringLiteral : (QUOTATION_MARK_CHAR (escapeQuot | ~QUOTATION_MARK_CHAR)* QUOTATION_MARK_CHAR) | (QUOTATION_MARK_CHAR (escapeApos | ~APOSTROPHE_CHAR)* QUOTATION_MARK_CHAR) ;

/** [99] URIQualifiedName ::= BracedURILiteral NCName
 *
 * //ws: explicit
 */
uRIQualifiedName : bracedURILiteral nCName ;

/** [100] BracedURILiteral ::= "Q" "{" [^{}]* "}"
 *
 * //ws: explicit
 */
bracedURILiteral : Q_CHAR LEFT_CURLY_BRACKET_CHAR ~(LEFT_CURLY_BRACKET_CHAR|RIGHT_CURLY_BRACKET_CHAR)* RIGHT_CURLY_BRACKET_CHAR ;

/** [101] EscapeQuot ::= '""' */
escapeQuot : QUOTATION_MARK_CHAR QUOTATION_MARK_CHAR ;
    
/** [102] EscapeApos ::= "''" */
escapeApos : APOSTROPHE_CHAR APOSTROPHE_CHAR ;
    
/** [103] Comment ::= "(:" (CommentContents | Comment)* ":)"
 *
 * //ws: explicit
 * //gn: comments
 */
comment : LEFT_PARENTHESIS_CHAR COLON_CHAR (CommentContents | comment)* COLON_CHAR RIGHT_PARENTHESIS_CHAR ;

// [105] NCName ::= [http://www.w3.org/TR/REC-xml-names/#NT-NCName]Names  /* ws: explicit */
// implemented in XML11.g4
    
// [106] Char ::= [http://www.w3.org/TR/REC-xml#NT-Char]XML    /* ws :explicit */
// implemented in XML11.g4
    
// The following symbols are used only in the definition of terminal symbols; they are not terminal symbols in the grammar of A.1 EBNF.
/** [107] Digits ::= [0-9]+ */
Digits : [0-9]+ ;

/** [108] CommentContents ::= (Char+ - (Char* ('(:' | ':)') Char*)) */
CommentContents : Char+ { System.out.println(getText());  } ; //TODO



FOR             : 'for';
RETURN          : 'return' ;
IN              : 'in' ;
LET             : 'let' ;
SOME            : 'some' ;
EVERY           : 'every' ;
SATISFIES       : 'satisfies' ;
IF              : 'if' ;
THEN            : 'then' ;
ELSE            : 'else' ;
OR              : 'or' ;
AND             : 'and' ;
TO              : 'to' ;
UNION           : 'union' ;
INTERSECT       : 'intersect' ;
EXCEPT          : 'except' ;
INSTANCE        : 'instrance' ;
OF              : 'of' ;
TREAT           : 'treat' ;
AS              : 'as' ;
CASTABLE        : 'castable' ;
CAST            : 'cast' ;
FUNCTION        : 'function' ;
EMPTY_SEQUENCE  : 'empty-sequence' ;
DIV             : 'div' ;
IDIV            : 'idiv' ;
MOD             : 'mod' ;

CHILD               : 'child' ;
DESCENDANT          : 'descendant' ;
ATTRIBUTE           : 'attribute' ;
SELF                : 'self' ;
DESCENDANT_OR_SELF  : 'descendant-or-self' ;
FOLLOWING_SIBLING   : 'following-sibling' ;
FOLLOWING           : 'following' ;
PARENT              : 'parent' ;
ANCESTOR            : 'ancestor' ;
ANCESTOR_OR_SELF    : 'ancestor-or-self' ;
PRECEDING_SIBLING   : 'preceding-sibling' ;
PRECEDING           : 'preceding' ;
IS                  : 'is' ;

NAMESPACE               : 'namespace' ;
ITEM                    : 'item' ;
NODE                    : 'node' ;
DOCUMENT_NODE           : 'document-node' ;
TEXT                    : 'text' ;
COMMENT                 : 'comment' ;
NAMESPACE_NODE          : 'namespace-node' ;
PROCESSING_INSTRUCTION  : 'processing-instruction' ;
SCHEMA_ELEMENT          : 'schema-element' ;
SCHEMA_ATTRIBUTE        : 'schema-attribute' ;
ELEMENT                 : 'element' ;

AXIS_NODE_SYMBOL    : '::' ;
ATTRIBUTE_SYMBOL    : COMMERCIAL_AT_CHAR ;

OR_SYMBOL           : '||' ;
PRECEDES_SYMBOL     : '<<' ;
FOLLOWS_SYMBOL      : '>>' ;
CHILDREN_SYMBOL     : SOLIDUS_CHAR ;
DESCENDANTS_SYMBOL  : '//' ;
REVERSE_SYMBOL      : '..' ;

EQUALS_SYMBOL                   : '=' ;
NOT_EQUALS_SYMBOL               : '!=' ;
LESS_THAN_SYMBOL                : '<' ;
LESS_THAN_OR_EQUALS_SYMBOL      : '<=' ; 
GREATER_THAN_SYMBOL             : '>' ;
GREATER_THAN_OR_EQUALS_SYMBOL   : '>=' ;

EQ  : 'eq' ;
NE  : 'ne' ;
LT  : 'lt' ;
LE  : 'le' ;
GT  : 'gt' ;
GE  : 'ge' ;

Q_CHAR                      : 'Q' ;
LEFT_CURLY_BRACKET_CHAR     : '{' ;
RIGHT_CURLY_BRACKET_CHAR    : '}' ;
DOLLAR_SIGN_CHAR            : '$' ;
COMMA_CHAR                  : ',' ;
LEFT_PARENTHESIS_CHAR       : '(' ;
RIGHT_PARENTHESIS_CHAR      : ')' ;
PLUS_SIGN_CHAR              : '+' ;
ASTERISK_CHAR               : '*' ;
VERTICAL_LINE_CHAR          : '|' ;
EXCLAMATION_MARK_CHAR       : '!' ;
SOLIDUS_CHAR                : '/' ;
COMMERCIAL_AT_CHAR          : '@' ;
LEFT_SQUARE_BRACKET_CHAR    : '[' ;
RIGHT_SQUARE_BRACKET_CHAR   : ']' ;
QUESTION_MARK_CHAR          : '?' ;
NUMBER_SIGN_CHAR            : '#' ;
QUOTATION_MARK_CHAR         : '"' ;
APOSTROPHE_CHAR             : '\u0027' ;
