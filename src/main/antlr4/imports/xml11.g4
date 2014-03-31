/*
    Antlr v4 Grammar for Extensible Markup Language (XML) 1.1
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
 * Antlr v4 Grammar for Extensible Markup Language (XML) 1.1
 * as defined by the W3C
 * http://www.w3.org/TR/xml11/#charsets
 *
 * @author Adam Retter <adam.retter@googlemail.com>
 */
grammar xml11;

/** [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF] */
NameStartChar : COLON_CHAR | 'A'..'Z' | UNDERSCORE_CHAR | 'a'..'z' | '\u00C0'..'\u00D6' | '\u00D8'..'\u00F6' | '\u00F8'..'\u02FF' | '\u0370'..'\u037D' | '\u037F'..'\u1FFF' | '\u200C'..'\u200D' | '\u2070'..'\u218F' | '\u2C00'..'\u2FEF' | '\u3001'..'\uD7FF' | '\uF900'..'\uFDCF' | '\uFDF0'..'\uFFFD' | '\u10000'..'\uEFFFF' ;

/** [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040] */
NameChar : NameStartChar | HYPEN_MINUS_CHAR | FULL_STOP_CHAR | '0'..'9' | '\u00B7' | '\u0300'..'\u036F' | '\u203F'..'\u2040' ;



COLON_CHAR : ':' ;
UNDERSCORE_CHAR : '_' ;
HYPEN_MINUS_CHAR : '-' ;
FULL_STOP_CHAR : '.' ;
