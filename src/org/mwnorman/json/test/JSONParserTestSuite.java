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
import java.io.PrintWriter;
import java.io.StringReader;
import java.math.BigDecimal;

//java eXtension imports (JSR-353)
import javax.json.JsonValue;
import javax.json.spi.JsonProvider;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParser.Event;
import javax.json.stream.JsonParsingException;

import static javax.json.stream.JsonParser.Event.END_ARRAY;
import static javax.json.stream.JsonParser.Event.END_OBJECT;
import static javax.json.stream.JsonParser.Event.KEY_NAME;
import static javax.json.stream.JsonParser.Event.START_ARRAY;
import static javax.json.stream.JsonParser.Event.START_OBJECT;
import static javax.json.stream.JsonParser.Event.VALUE_FALSE;
import static javax.json.stream.JsonParser.Event.VALUE_NULL;
import static javax.json.stream.JsonParser.Event.VALUE_NUMBER;
import static javax.json.stream.JsonParser.Event.VALUE_STRING;
import static javax.json.stream.JsonParser.Event.VALUE_TRUE;

//JUnit4 imports
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

//my parser imports
import org.mwnorman.json.JSONParser;
import org.mwnorman.json.JSONParserDisplayer;
import org.mwnorman.json.ParseException;

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

    static boolean isValueEvent(Event event) {
        if (event.ordinal() > KEY_NAME.ordinal() && event.ordinal() < END_OBJECT.ordinal()) {
            return true;
        }
        return false;
    }
    static boolean isEndEvent(Event event) {
        if (event == END_ARRAY || event == END_OBJECT) {
            return true;
        }
        return false;
    }
    static boolean isStartEvent(Event event) {
        if (event == START_ARRAY || event == START_OBJECT) {
            return true;
        }
        return false;
    }
    static void displayJsonEvent(PrintWriter pw, Event event, JsonParser parser) {
        StringBuilder sb = new StringBuilder();
        int lineNumber = parser.getLocation().getLineNumber();
        if (lineNumber != -1) {
            sb.append("(");
            sb.append(lineNumber);
            sb.append("lx");
            sb.append(parser.getLocation().getColumnNumber());
            sb.append("c)");
        }
        switch (event) {
            case END_ARRAY:
                sb.append("]");
                break;
            case END_OBJECT:
                sb.append("}");
                break;
            case KEY_NAME:
                sb.append("\"");
                sb.append(parser.getString());
                sb.append("\":");
                break;
            case START_ARRAY:
                sb.append("[");
                break;
            case START_OBJECT:
                sb.append("{");
                break;
            case VALUE_FALSE:
                sb.append(JsonValue.FALSE);
                break;
            case VALUE_NULL:
                sb.append(JsonValue.NULL);
                break;
            case VALUE_NUMBER:
                BigDecimal bd = parser.getBigDecimal();
                if (bd != null) {
                    sb.append(bd);
                }
                else {
                    long l = parser.getLong();
                    int i = parser.getInt();
                    if (l == 0 && i !=0) {
                        sb.append(i);
                    }
                    else {
                        sb.append(l);
                    }
                }
                break;
            case VALUE_STRING:
                sb.append("\"");
                sb.append(parser.getString());
                sb.append("\"");
                break;
            case VALUE_TRUE:
                sb.append(JsonValue.TRUE.toString());
                break;
            default:
                break;
        }
        pw.print(sb.toString());
        pw.flush();
    }

    //JUnit4 fixtures
    static JsonProvider provider  = null;

    @BeforeClass
    public static void setUp() {
        try {
            provider = JsonProvider.provider();
        }
        catch (JsonParsingException jpe) {
            fail(jpe.getMessage());
        }
    }

    static final String TEST_JSON_STR1 =
        "{" +
            "\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25, " +
            "\"phoneNumber\":[" +
                "{\"type\": \"home\", \"number\": \"212 555-1234\"}," +
                "{\"type\": \"fax\", \"number\": \"646 555-4567\"}" +
            "]" +
        "}";
    @Ignore
    public void getMyParser() {
        StringReader stringReader = new StringReader(TEST_JSON_STR1);
        JsonParser parser = provider.createParser(stringReader);
        JSONParserDisplayer jsonDisplayer = new JSONParserDisplayer((JSONParser)parser);
        jsonDisplayer.display(new PrintWriter(System.out));
        //use JSR-353 API to re-create JSONParserDisplayer
        PrintWriter pw = new PrintWriter(System.out);
        while (parser.hasNext()) {
            Event event = parser.next();
            displayJsonEvent(pw, event, parser);
            if (parser.hasNext()) {
                Event nextevent = ((JSONParser)parser).peek(); //cheat!
                if (!(nextevent == Event.END_ARRAY ||
                    nextevent == Event.END_OBJECT ||
                    nextevent == Event.START_OBJECT ||
                    nextevent == Event.START_ARRAY ||
                    nextevent == Event.KEY_NAME)) {
                    pw.print(" ");
                }
                else if (event == Event.VALUE_STRING && nextevent == Event.KEY_NAME) {
                    pw.print(", ");
                }
                else if (event == Event.VALUE_NUMBER  && nextevent == Event.KEY_NAME) {
                    pw.print(", ");
                }
                else if ((event == Event.END_OBJECT || event == Event.END_ARRAY)  &&
                    (nextevent == Event.START_OBJECT || nextevent == Event.START_ARRAY)) {
                    pw.print(",");
                }
            }
        }
        pw.flush();
    }
    @Test
    public void trueAtom() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + JsonValue.TRUE.toString() +
            CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void falseAtom() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + JsonValue.FALSE.toString() +
            CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_FALSE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void Ccomment1() {
        StringReader stringReader =
                new StringReader(C_COMMENT_START + "stuff to ignore" + C_COMMENT_END +
                    OPEN_BRACKET + JsonValue.TRUE.toString() + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void Ccomment2() {
        StringReader stringReader =
                new StringReader(OPEN_BRACKET + C_COMMENT_START + "stuff to ignore" + C_COMMENT_END +
                    JsonValue.TRUE.toString() + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void Ccomment3() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + JsonValue.TRUE.toString() +
            C_COMMENT_START + "stuff to ignore" + C_COMMENT_END + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void Ccomment4() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + JsonValue.TRUE.toString() +
            CLOSE_BRACKET + C_COMMENT_START + "stuff to ignore" + C_COMMENT_END);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }


    @Test
    public void nestedCcomment() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + JsonValue.TRUE.toString() +
            CLOSE_BRACKET + C_COMMENT_START + "stuff to ignore\n" + C_COMMENT_START +
            "more stuff to ignore\n" + C_COMMENT_END + C_COMMENT_END);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void HTMLcomment1() {
        StringReader stringReader = new StringReader(HTML_COMMENT_START + "stuff to ignore" +
            HTML_COMMENT_END + OPEN_BRACKET + JsonValue.TRUE.toString() + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void HTMLcomment2() {
        StringReader stringReader = new StringReader("[" + HTML_COMMENT_START + "stuff to ignore" +
            HTML_COMMENT_END + JsonValue.TRUE.toString() + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void HTMLcomment3() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + JsonValue.TRUE.toString() +
            HTML_COMMENT_START + "stuff to ignore" + HTML_COMMENT_END + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void HTMLcomment4() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + JsonValue.TRUE.toString() +
            CLOSE_BRACKET + HTML_COMMENT_START + "stuff to ignore" + HTML_COMMENT_END);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_TRUE, e2);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    static final String ANSWER = "42";
    @Test
    public void numberAtom1() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + ANSWER + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_NUMBER, e2);
        Integer atom = parser.getInt();
        assertNotNull(atom);
        assertEquals(Integer.valueOf(42), atom);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
    }

    @Test
    public void extraComma() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + ANSWER + COMMA + CLOSE_BRACKET);
        boolean worked = true;
        String msg = "";
        @SuppressWarnings("unused")JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
        }
        catch (JsonParsingException jpe) {
            worked = false;
            msg = jpe.getMessage();
        }
        assertFalse(worked);
        assertTrue(msg.contains("Encountered") && msg.contains("expecting"));
    }

    @Test
    public void emptyElement() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + ANSWER + COMMA + COMMA +
            CLOSE_BRACKET);
        boolean worked = true;
        String msg = "";
        @SuppressWarnings("unused")JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
        }
        catch (JsonParsingException jpe) {
            worked = false;
            msg = jpe.getMessage();
        }
        assertFalse(worked);
        assertTrue(msg.contains("Encountered") && msg.contains("expecting"));
    }

    static final String REALLY_BIG_INT = "12345678901234567890123456789012345678901234567890";
    @Test
    public void numberAtom2() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + REALLY_BIG_INT + CLOSE_BRACKET);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(VALUE_NUMBER, e2);
        int i = parser.getInt();
        assertEquals(0, i);
        long l = parser.getLong();
        assertEquals(0l, l);
        BigDecimal bd = parser.getBigDecimal();
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
        BigDecimal atom = new BigDecimal(REALLY_BIG_INT);
        assertEquals(bd, atom);
    }

    /*
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
*/
}