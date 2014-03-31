/*
    Antlr v4 Grammar for Namespaces in XML 1.1
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
 * Antlr v4 Grammar for Namespaces in XML 1.1
 * as defined by the W3C
 * http://www.w3.org/TR/xml-names11/
 *
 * @author Adam Retter <adam.retter@googlemail.com>
 */
grammar xmlnames11;

import xml11;


/** [4] NCName ::= NCNameStartChar NCNameChar*	//An XML Name, minus the ":" */
nCName : nCNameStartChar nCNameChar ;

/** [5] NCNameChar ::= NameChar - ':' */
nCNameChar : NameChar ~ COLON_CHAR ;

/** [6] NCNameStartChar ::= NameStartChar - ':' */
nCNameStartChar : NameStartChar ~ COLON_CHAR ;

/**
 * [7] QName ::= PrefixedName
 * | UnprefixedName
 */
qName : prefixedName
      | unprefixedName ;

/** [8] PrefixedName ::= Prefix ':' LocalPart **/
prefixedName : prefix COLON_CHAR localPart ;

/** [9] UnprefixedName ::= LocalPart */
unprefixedName : localPart ;

/** [10] Prefix ::= NCName */
prefix : nCName ;

/** [11] LocalPart ::= NCName */
localPart : nCName ;


COLON_CHAR : ':' ;
