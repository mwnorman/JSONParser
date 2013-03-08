/*
 * This software is licensed under the terms of the ISC License.
 * (ISCL http://www.opensource.org/licenses/isc-license.txt
 * It is functionally equivalent to the 2-clause BSD licence,
 * with language "made unnecessary by the Berne convention" removed).
 *
 * Copyright (c) 2011-2013 Mike Norman
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 * USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 ******************************************************************************/
package org.mwnorman.json.test;

//javase imports
import java.io.StringReader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.Map;
import java.util.Set;

//JUnit4 imports
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;

//domain-specific imports
import org.mwnorman.json.JSONParser;
import org.mwnorman.json.ParseException;
import static org.mwnorman.json.JSONParser.TRUE_ATOM;
import static org.mwnorman.json.JSONParser.FALSE_ATOM;

public class JSONParserTestSuite {

    static final String COLON = ":";
    static final String COMMA = ",";
    static final String DOLLAR_SIGN = "$";
    static final String QUOTE = "\"";
    static final String OPEN_BRACE = "{";
    static final String CLOSE_BRACE = "}";
    static final String OPEN_BRACKET = "[";
    static final String CLOSE_BRACKET = "]";
    static final String DOUBLE_SLASH = "\\\\";
    static final String C_COMMENT_START = "/*";
    static final String C_COMMENT_END = "*/";
    static final String HTML_COMMENT_START = "<!--";
    static final String HTML_COMMENT_END = "-->";
    static final String KEY = "key";
    static final String VALUE = "value";

    @Test
    public void trueAtom() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + TRUE_ATOM + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom);
    }
    
    @Test
    public void falseAtom() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + FALSE_ATOM + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.FALSE, atom);
    }
    
    @Test
    public void Ccomment1() {
        StringReader stringReader = 
                new StringReader(C_COMMENT_START + "stuff to ignore" + C_COMMENT_END +
                    OPEN_BRACKET + TRUE_ATOM + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void Ccomment2() {
        StringReader stringReader = 
                new StringReader(OPEN_BRACKET + C_COMMENT_START + "stuff to ignore" + C_COMMENT_END +
                    TRUE_ATOM + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void Ccomment3() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + TRUE_ATOM + C_COMMENT_START +
            "stuff to ignore" + C_COMMENT_END + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void Ccomment4() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + TRUE_ATOM + CLOSE_BRACKET +
            C_COMMENT_START + "stuff to ignore" + C_COMMENT_END);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void nestedCcomment() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + TRUE_ATOM + CLOSE_BRACKET +
            C_COMMENT_START + "stuff to ignore\n" + C_COMMENT_START +
            "more stuff to ignore\n" + C_COMMENT_END + C_COMMENT_END);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void HTMLcomment1() {
        StringReader stringReader = new StringReader(HTML_COMMENT_START + "stuff to ignore" + 
            HTML_COMMENT_END + OPEN_BRACKET + TRUE_ATOM + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void HTMLcomment2() {
        StringReader stringReader = new StringReader("[" + HTML_COMMENT_START + "stuff to ignore" +
            HTML_COMMENT_END + TRUE_ATOM + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void HTMLcomment3() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + TRUE_ATOM + HTML_COMMENT_START +
            "stuff to ignore" + HTML_COMMENT_END + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }
    
    @Test
    public void HTMLcomment4() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + TRUE_ATOM + CLOSE_BRACKET +
            HTML_COMMENT_START + "stuff to ignore" + HTML_COMMENT_END);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Boolean> parsedJSON = null;
        try {
            parsedJSON = parser.<Boolean>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Boolean atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom); 
    }

    static final String ANSWER = "42";
    @Test
    public void numberAtom1() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + ANSWER + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<Integer> parsedJSON = null;
        try {
            parsedJSON = parser.<Integer>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Integer atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertEquals(Integer.valueOf(42), atom);
    }

    @Test
    public void extraComma() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + ANSWER + COMMA + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        @SuppressWarnings("unused")List<Integer> parsedJSON = null;
        try {
            parsedJSON = parser.<Integer>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertFalse(worked);
    }
    
    @Test
    public void emptyElement() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + ANSWER + COMMA + COMMA + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        @SuppressWarnings("unused")List<Integer> parsedJSON = null;
        try {
            parsedJSON = parser.<Integer>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertFalse(worked);
    }
    
    static final String REALLY_BIG_INT = "12345678901234567890123456789012345678901234567890";
    @Test
    public void numberAtom2() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + REALLY_BIG_INT + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<BigInteger> parsedJSON = null;
        try {
            parsedJSON = parser.<BigInteger>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        BigInteger atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertEquals(new BigInteger(REALLY_BIG_INT), atom);
    }

    static final String REALLY_BIG_DECIMAL = "123456789012345678901.23456789012345678901234567890";
    @Test
    public void numberAtom3() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + REALLY_BIG_DECIMAL + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<BigDecimal> parsedJSON = null;
        try {
            parsedJSON = parser.<BigDecimal>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        BigDecimal atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertEquals(new BigDecimal(REALLY_BIG_DECIMAL), atom);
    }

    static final String LEADING_ZEROS = "00.10";
    @Test
    public void numberAtom4() {
        //extra leading zero should NOT throw an exception
        StringReader stringReader = new StringReader(OPEN_BRACKET + LEADING_ZEROS + CLOSE_BRACKET);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        List<BigDecimal> parsedJSON = null;
        try {
            parsedJSON = parser.<BigDecimal>array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        BigDecimal atom = parsedJSON.get(0);
        assertNotNull(atom);
        assertEquals(new BigDecimal("00.10"), atom);
    }
    
    static final String EMPTY_JSON_OBJECT ="{}";
    @Test
    public void emtyMap() {
        StringReader stringReader = new StringReader(EMPTY_JSON_OBJECT);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Map<String, Object> parsedJSON = null;
        try {
            parsedJSON = parser.object();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        assertTrue(parsedJSON.isEmpty());
    }
    
    static final String NO_DOUBLE_SLASH_IN_KEY =
        OPEN_BRACE +
            QUOTE + KEY + QUOTE + ":" + QUOTE + VALUE + QUOTE +
        CLOSE_BRACE;
    @Test
    public void noDoubleSlashAtEnd() {
        StringReader stringReader = new StringReader(NO_DOUBLE_SLASH_IN_KEY);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Map<String, String> parsedJSON = null;
        try {
            parsedJSON = parser.<String>object();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        String k = parsedJSON.get(KEY);
        assertEquals(VALUE, k);
    }

    static final String DOUBLE_SLASH_IN_KEY =
        OPEN_BRACE +
            QUOTE + KEY + DOUBLE_SLASH + QUOTE + ":" + QUOTE + VALUE + QUOTE +
        CLOSE_BRACE;
    @Test
    public void doubleSlashAtEnd() {
        StringReader stringReader = new StringReader(DOUBLE_SLASH_IN_KEY);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Map<String, String> parsedJSON = null;
        try {
            parsedJSON = parser.<String>object();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        String k = parsedJSON.keySet().iterator().next();
        assertTrue(k.endsWith(DOUBLE_SLASH));
    }

    static final String ATTRS = "Attrs.Attr";
    static final String DOLLAR_SIGN_IN_KEY =
        "{'" + ATTRS + "':{'" + DOLLAR_SIGN + "all" +
            "':[{\"" + DOLLAR_SIGN + "elemMatch\":{'Name':'srv','Value':'srvId'}}]}}";
    @Test
    public void dollarSignInKey() {
        StringReader stringReader = new StringReader(DOLLAR_SIGN_IN_KEY);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        @SuppressWarnings("rawtypes")Map parsedJSON = null;
        try {
            parsedJSON = parser.object();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        @SuppressWarnings("rawtypes")Map innerMap = (Map)parsedJSON.get(ATTRS);
        String dollarAllKey = (String)innerMap.keySet().iterator().next();
        assertTrue(dollarAllKey.startsWith(DOLLAR_SIGN));
    }

    static final String NAKED_KEY = "bike";
    static final String NAKED_VALUE = "harley";
    @Test
    public void nakedStrings() {
        StringReader stringReader = new StringReader(OPEN_BRACE + NAKED_KEY + COLON + NAKED_VALUE + CLOSE_BRACE);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Map<String,String> parsedJSON = null;
        try {
            parsedJSON = parser.<String>object();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        String k = parsedJSON.keySet().iterator().next();
        assertEquals(k, NAKED_KEY);
        assertEquals(parsedJSON.get(NAKED_KEY), NAKED_VALUE);
    }
    static final String NESTED_ARRAY =
        "[0,{\"1\":{\"2\":{\"3\":{\"4\":[5,{\"6\":7}]}}}}]";
    @Test
    public void nestedMaps() {
        StringReader stringReader = new StringReader(NESTED_ARRAY);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        @SuppressWarnings("rawtypes")List parsedJSON = null;
        try {
            parsedJSON = parser.array();
        } catch (ParseException e) {
            worked = false;
        }
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Integer firstValue = (Integer)parsedJSON.get(0);
        assertEquals(0L, firstValue.longValue());
        @SuppressWarnings("rawtypes")Map firstMap = (Map)parsedJSON.get(1);
        assertNotNull(firstMap);
        @SuppressWarnings("rawtypes")Set firstKeys = firstMap.keySet();
        assertTrue(firstKeys.size() == 1);
        String firstKey = (String)firstKeys.iterator().next();
        assertEquals("1", firstKey);
    }

}