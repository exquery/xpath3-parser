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
grammar xpath3;

import xmlnames11;


/** [1] XPath ::= Expr */
xpath : expr ;

/*
[2]   	ParamList	   ::=   	Param ("," Param)*
[3]   	Param	   ::=   	"$" EQName TypeDeclaration?
[4]   	FunctionBody	   ::=   	EnclosedExpr
*/

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
           | letExpr /*
           | quantifiedExpr
           | ifExpr
           | orExpr*/ ;

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

/*
[16]   	OrExpr	   ::=   	AndExpr ( "or" AndExpr )*
[17]   	AndExpr	   ::=   	ComparisonExpr ( "and" ComparisonExpr )*
[18]   	ComparisonExpr	   ::=   	StringConcatExpr ( (ValueComp
| GeneralComp
| NodeComp) StringConcatExpr )?
[19]   	StringConcatExpr	   ::=   	RangeExpr ( "||" RangeExpr )*
[20]   	RangeExpr	   ::=   	AdditiveExpr ( "to" AdditiveExpr )?
[21]   	AdditiveExpr	   ::=   	MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
[22]   	MultiplicativeExpr	   ::=   	UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
[23]   	UnionExpr	   ::=   	IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*
[24]   	IntersectExceptExpr	   ::=   	InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*
[25]   	InstanceofExpr	   ::=   	TreatExpr ( "instance" "of" SequenceType )?
[26]   	TreatExpr	   ::=   	CastableExpr ( "treat" "as" SequenceType )?
[27]   	CastableExpr	   ::=   	CastExpr ( "castable" "as" SingleType )?
[28]   	CastExpr	   ::=   	UnaryExpr ( "cast" "as" SingleType )?
[29]   	UnaryExpr	   ::=   	("-" | "+")* ValueExpr
[30]   	ValueExpr	   ::=   	SimpleMapExpr
[31]   	GeneralComp	   ::=   	"=" | "!=" | "<" | "<=" | ">" | ">="
[32]   	ValueComp	   ::=   	"eq" | "ne" | "lt" | "le" | "gt" | "ge"
[33]   	NodeComp	   ::=   	"is" | "<<" | ">>"
[34]   	SimpleMapExpr	   ::=   	PathExpr ("!" PathExpr)*
[35]   	PathExpr	   ::=   	("/" RelativePathExpr?)
| ("//" RelativePathExpr)
| RelativePathExpr
[36]   	RelativePathExpr	   ::=   	StepExpr (("/" | "//") StepExpr)*
[37]   	StepExpr	   ::=   	PostfixExpr | AxisStep
[38]   	AxisStep	   ::=   	(ReverseStep | ForwardStep) PredicateList
[39]   	ForwardStep	   ::=   	(ForwardAxis NodeTest) | AbbrevForwardStep
*/

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
                ) '::' ;

/*[41]   	AbbrevForwardStep	   ::=   	"@"? NodeTest
[42]   	ReverseStep	   ::=   	(ReverseAxis NodeTest) | AbbrevReverseStep
[43]   	ReverseAxis	   ::=   	("parent" "::")
| ("ancestor" "::")
| ("preceding-sibling" "::")
| ("preceding" "::")
| ("ancestor-or-self" "::")
[44]   	AbbrevReverseStep	   ::=   	".."
[45]   	NodeTest	   ::=   	KindTest | NameTest
[46]   	NameTest	   ::=   	EQName | Wildcard
[47]   	Wildcard	   ::=   	"*"
| (NCName ":" "*")
| ("*" ":" NCName)
| (BracedURILiteral "*")
[48]   	PostfixExpr	   ::=   	PrimaryExpr (Predicate | ArgumentList)*
[49]   	ArgumentList	   ::=   	"(" (Argument ("," Argument)*)? ")"
[50]   	PredicateList	   ::=   	Predicate*
[51]   	Predicate	   ::=   	"[" Expr "]"
[52]   	PrimaryExpr	   ::=   	Literal
| VarRef
| ParenthesizedExpr
| ContextItemExpr
| FunctionCall
| FunctionItemExpr
[53]   	Literal	   ::=   	NumericLiteral | StringLiteral
[54]   	NumericLiteral	   ::=   	IntegerLiteral | DecimalLiteral | DoubleLiteral
[55]   	VarRef	   ::=   	"$" VarName
*/

/** [56] VarName ::= EQName */
varName : eQName ;

/*
[57]   	ParenthesizedExpr	   ::=   	"(" Expr? ")"
[58]   	ContextItemExpr	   ::=   	"."
[59]   	FunctionCall	   ::=   	EQName ArgumentList

[60]   	Argument	   ::=   	ExprSingle | ArgumentPlaceholder
[61]   	ArgumentPlaceholder	   ::=   	"?"
[62]   	FunctionItemExpr	   ::=   	NamedFunctionRef | InlineFunctionExpr
[63]   	NamedFunctionRef	   ::=   	EQName "#" IntegerLiteral	
[64]   	InlineFunctionExpr	   ::=   	"function" "(" ParamList? ")" ("as" SequenceType)? FunctionBody
[65]   	SingleType	   ::=   	SimpleTypeName "?"?
[66]   	TypeDeclaration	   ::=   	"as" SequenceType
[67]   	SequenceType	   ::=   	("empty-sequence" "(" ")")
| (ItemType OccurrenceIndicator?)
[68]   	OccurrenceIndicator	   ::=   	"?" | "*" | "+"	
[69]   	ItemType	   ::=   	KindTest | ("item" "(" ")") | FunctionTest | AtomicOrUnionType | ParenthesizedItemType
[70]   	AtomicOrUnionType	   ::=   	EQName
[71]   	KindTest	   ::=   	DocumentTest
| ElementTest
| AttributeTest
| SchemaElementTest
| SchemaAttributeTest
| PITest
| CommentTest
| TextTest
| NamespaceNodeTest
| AnyKindTest
[72]   	AnyKindTest	   ::=   	"node" "(" ")"
[73]   	DocumentTest	   ::=   	"document-node" "(" (ElementTest | SchemaElementTest)? ")"
[74]   	TextTest	   ::=   	"text" "(" ")"
[75]   	CommentTest	   ::=   	"comment" "(" ")"
[76]   	NamespaceNodeTest	   ::=   	"namespace-node" "(" ")"
[77]   	PITest	   ::=   	"processing-instruction" "(" (NCName | StringLiteral)? ")"
[78]   	AttributeTest	   ::=   	"attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"
[79]   	AttribNameOrWildcard	   ::=   	AttributeName | "*"
[80]   	SchemaAttributeTest	   ::=   	"schema-attribute" "(" AttributeDeclaration ")"
[81]   	AttributeDeclaration	   ::=   	AttributeName
[82]   	ElementTest	   ::=   	"element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"
[83]   	ElementNameOrWildcard	   ::=   	ElementName | "*"
[84]   	SchemaElementTest	   ::=   	"schema-element" "(" ElementDeclaration ")"
[85]   	ElementDeclaration	   ::=   	ElementName
[86]   	AttributeName	   ::=   	EQName
[87]   	ElementName	   ::=   	EQName
[88]   	SimpleTypeName	   ::=   	TypeName
[89]   	TypeName	   ::=   	EQName
[90]   	FunctionTest	   ::=   	AnyFunctionTest
| TypedFunctionTest
[91]   	AnyFunctionTest	   ::=   	"function" "(" "*" ")"
[92]   	TypedFunctionTest	   ::=   	"function" "(" (SequenceType ("," SequenceType)*)? ")" "as" SequenceType
[93]   	ParenthesizedItemType	   ::=   	"(" ItemType ")"
*/

/** [94] EQName ::= QName | URIQualifiedName */
eQName : qName | uRIQualifiedName ;

/*
[95]   	IntegerLiteral	   ::=   	Digits
[96]   	DecimalLiteral	   ::=   	("." Digits) | (Digits "." [0-9]*)
[97]   	DoubleLiteral	   ::=   	(("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
[98]   	StringLiteral	   ::=   	('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
*/

/** [99] URIQualifiedName ::= BracedURILiteral NCName */
uRIQualifiedName : bracedURILiteral nCName ;

/** [100] BracedURILiteral ::= "Q" "{" [^{}]* "}" */
bracedURILiteral : Q_CHAR LEFT_CURLY_BRACKET_CHAR ~(LEFT_CURLY_BRACKET_CHAR|RIGHT_CURLY_BRACKET_CHAR)* RIGHT_CURLY_BRACKET_CHAR ;


/*
[101]   	EscapeQuot	   ::=   	'""'
[102]   	EscapeApos	   ::=   	"''"
[103]   	Comment	   ::=   	"(:" (CommentContents | Comment)* ":)"
*/

/*
[105]   	NCName	   ::=   	[http://www.w3.org/TR/REC-xml-names/#NT-NCName]Names
[106]   	Char	   ::=   	[http://www.w3.org/TR/REC-xml#NT-Char]XML	
The following symbols are used only in the definition of terminal symbols; they are not terminal symbols in the grammar of A.1 EBNF.
[107]   	Digits	   ::=   	[0-9]+
[108]   	CommentContents	   ::=   	(Char+ - (Char* ('(:' | ':)') Char*))
*/



FOR : 'for';
RETURN  : 'return' ;
IN : 'in' ;
LET : 'let' ;
SOME : 'some' ;
EVERY : 'every' ;
SATISFIES : 'satisfies' ;
IF : 'if' ;
THEN: 'then' ;
ELSE: 'else' ;

CHILD               : 'child' ;
DESCENDANT          : 'descendant' ;
ATTRIBUTE           : 'attribute' ;
SELF                : 'self' ;
DESCENDANT_OR_SELF  : 'descendant-or-self' ;
FOLLOWING_SIBLING   : 'following-sibling' ;
FOLLOWING           : 'following' ;
NAMESPACE           : 'namespace' ;

Q_CHAR: 'Q' ;
LEFT_CURLY_BRACKET_CHAR : '{' ;
RIGHT_CURLY_BRACKET_CHAR : '}' ;
DOLLAR_SIGN_CHAR : '$' ;
COMMA_CHAR : ',' ;
LEFT_PARENTHESIS_CHAR : '(' ;
RIGHT_PARENTHESIS_CHAR : ')' ;
