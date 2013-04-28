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
import java.io.ByteArrayInputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.nio.charset.Charset;

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
//import org.junit.Ignore;
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
    @Test
    public void getMyParser() {
        StringReader stringReader = new StringReader(TEST_JSON_STR1);
        JsonParser parser = provider.createParser(stringReader);
        JSONParserDisplayer jsonDisplayer = new JSONParserDisplayer((JSONParser)parser);
        StringWriter sw = new StringWriter();
        jsonDisplayer.display(new PrintWriter(sw));
        assertEquals(TEST_JSON_STR1, sw.toString());
    }

    @Test
    public void emptyObject() {
        ByteArrayInputStream bin = new ByteArrayInputStream("{}".getBytes());
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyObject(parser);
    }

    @Test
    public void emptyArray() {
        ByteArrayInputStream bin = new ByteArrayInputStream("[]".getBytes());
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyArray(parser);
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
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
        assertFalse(parser.hasNext());
    }

    static final String REALLY_BIG_DECIMAL = "123456789012345678901.23456789012345678901234567890";
    @Test
    public void numberAtom3() {
        StringReader stringReader = new StringReader(OPEN_BRACKET + REALLY_BIG_DECIMAL + CLOSE_BRACKET);
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
        BigDecimal rbd = parser.getBigDecimal();
        assertNotNull(rbd);
        assertEquals(new BigDecimal(REALLY_BIG_DECIMAL), rbd);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
        assertFalse(parser.hasNext());
    }

    static final String LEADING_ZEROS = "00.10";
    @Test
    public void numberAtom4() {
        //extra leading zero should NOT throw an exception
        StringReader stringReader = new StringReader(OPEN_BRACKET + LEADING_ZEROS + CLOSE_BRACKET);
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
        BigDecimal leadingZeros = parser.getBigDecimal();
        assertNotNull(leadingZeros);
        assertEquals(new BigDecimal(LEADING_ZEROS), leadingZeros);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
        assertFalse(parser.hasNext());
    }

    static final String EMPTY_JSON_OBJECT ="{}";
    @Test
    public void emptyJsonObject() {
        StringReader stringReader = new StringReader(EMPTY_JSON_OBJECT);
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(stringReader);
            worked = true;
        }
        catch (JsonParsingException jpe) {
        }
        assertTrue(worked);
        testEmptyObject(parser);
    }

    static final String NO_DOUBLE_SLASH_IN_KEY =
        OPEN_BRACE +
            QUOTE + KEY + QUOTE + ":" + QUOTE + VALUE + QUOTE +
        CLOSE_BRACE;
    @Test
    public void noDoubleSlashAtEnd() {
        StringReader stringReader = new StringReader(NO_DOUBLE_SLASH_IN_KEY);
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
        assertSame(START_OBJECT, e1);
        Event e2 = parser.next();
        assertSame(KEY_NAME, e2);
        String key = parser.getString();
        assertEquals(KEY, key);
        Event e3 = parser.next();
        assertSame(VALUE_STRING, e3);
        String value = parser.getString();
        assertEquals(VALUE, value);
        Event e4 = parser.next();
        assertSame(END_OBJECT, e4);
        assertFalse(parser.hasNext());
    }

    static final String DOUBLE_SLASH_IN_KEY =
        OPEN_BRACE +
            QUOTE + KEY + DOUBLE_SLASH + QUOTE + ":" + QUOTE + VALUE + QUOTE +
        CLOSE_BRACE;
    @Test
    public void doubleSlashAtEnd() {
        StringReader stringReader = new StringReader(DOUBLE_SLASH_IN_KEY);
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
        assertSame(START_OBJECT, e1);
        Event e2 = parser.next();
        assertSame(KEY_NAME, e2);
        String key = parser.getString();
        assertEquals(KEY+DOUBLE_SLASH, key);
        Event e3 = parser.next();
        assertSame(VALUE_STRING, e3);
        String value = parser.getString();
        assertEquals(VALUE, value);
        Event e4 = parser.next();
        assertSame(END_OBJECT, e4);
        assertFalse(parser.hasNext());
    }

    static final String ATTRS = "Attrs.Attr";
    static final String DOLLAR_SIGN_IN_KEY =
        "{'" + ATTRS + "':{'" + DOLLAR_SIGN + "all" +
            "':[{\"" + DOLLAR_SIGN + "elemMatch\":{'Name':'srv','Value':'srvId'}}]}}";
    @Test
    public void dollarSignInKey() {
        StringReader stringReader = new StringReader(DOLLAR_SIGN_IN_KEY);
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
        assertSame(START_OBJECT, e1);
        Event e2 = parser.next();
        assertSame(KEY_NAME, e2);
        String attrsKey = parser.getString();
        assertTrue(attrsKey.startsWith(ATTRS));
        Event e3 = parser.next();
        assertSame(START_OBJECT, e3);
        Event e4 = parser.next();
        assertSame(KEY_NAME, e4);
        String dollarAllKey = parser.getString();
        assertEquals(DOLLAR_SIGN + "all", dollarAllKey);
    }

    static final String NAKED_KEY = "bike";
    static final String NAKED_VALUE = "harley";
    @Test
    public void nakedStrings() {
        StringReader stringReader = new StringReader(OPEN_BRACE + NAKED_KEY + COLON + NAKED_VALUE +
            CLOSE_BRACE);
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
        assertSame(START_OBJECT, e1);
        Event e2 = parser.next();
        assertSame(KEY_NAME, e2);
        String nakedKey = parser.getString();
        assertEquals(nakedKey, NAKED_KEY);
        Event e3 = parser.next();
        assertSame(VALUE_STRING, e3);
        String nakedValue = parser.getString();
        assertEquals(nakedValue, NAKED_VALUE);
        Event e4 = parser.next();
        assertSame(END_OBJECT, e4);
        assertFalse(parser.hasNext());
    }

    static final String NESTED_ARRAY = "[0,{\"1\":{\"2\":{\"3\":{\"4\":[5,{\"6\":7}]}}}}]";
    @Test
    public void nestedMaps() {
        StringReader stringReader = new StringReader(NESTED_ARRAY);
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
    }

    //Unicode testing

    static final Charset UTF_8 = Charset.forName("UTF-8");
    static final Charset UTF_16BE = Charset.forName("UTF-16BE");
    static final Charset UTF_16LE = Charset.forName("UTF-16LE");
    static final Charset UTF_16 = Charset.forName("UTF-16");
    static final Charset UTF_32LE = Charset.forName("UTF-32LE");
    static final Charset UTF_32BE = Charset.forName("UTF-32BE");

    static final String ESCPD_STR1 = "[\"\\u0000\\u00ff\u00ff\"]";
    @Test
    public void testEscapedString1() throws Exception {
        StringReader stringReader = new StringReader(ESCPD_STR1);
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
        assertSame(VALUE_STRING, e2);
        String str = parser.getString();
        assertEquals("\u0000\u00ff\u00ff", str);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
        assertFalse(parser.hasNext());
    }

    static final String ESCPD_STR2 = "[\"\\u0000\"]";
    @Test
    public void testEscapedString2() throws Exception {
        StringReader stringReader = new StringReader(ESCPD_STR2);
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
        assertSame(VALUE_STRING, e2);
        String str = parser.getString();
        assertEquals("\u0000", str);
        Event e3 = parser.next();
        assertSame(END_ARRAY, e3);
        assertFalse(parser.hasNext());
    }

    @Test
    public void testEmptyArrayStreamUTF8() {
        ByteArrayInputStream bin = new ByteArrayInputStream("[]".getBytes(UTF_8));
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyArray(parser);
    }

    @Test
    public void testEmptyArrayStreamUTF16LE() {
        ByteArrayInputStream bin = new ByteArrayInputStream("[]".getBytes(UTF_16LE));
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyArray(parser);
    }

    @Test
    public void testEmptyArrayStreamUTF16BE() {
        ByteArrayInputStream bin = new ByteArrayInputStream("[]".getBytes(UTF_16BE));
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyArray(parser);
    }

    @Test
    public void testEmptyArrayStreamUTF32LE() {
        ByteArrayInputStream bin = new ByteArrayInputStream("[]".getBytes(UTF_32LE));
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyArray(parser);
    }

    @Test
    public void testEmptyArrayStreamUTF32BE() {
        ByteArrayInputStream bin = new ByteArrayInputStream("[]".getBytes(UTF_32BE));
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyArray(parser);
    }

    @Test
    public void testEmptyArrayStreamUTF16() {
        ByteArrayInputStream bin = new ByteArrayInputStream("[]".getBytes(UTF_16));
        boolean worked = false;
        JsonParser parser = null;
        try {
            parser = provider.createParser(bin);
            worked = true;
        }
        catch (JsonParsingException jpe) {
            System.identityHashCode(this);
        }
        assertTrue(worked);
        testEmptyArray(parser);
    }

    protected void testEmptyObject(JsonParser parser) {
        Event e1 = parser.next();
        assertSame(START_OBJECT, e1);
        Event e2 = parser.next();
        assertSame(END_OBJECT, e2);
        assertFalse(parser.hasNext());
    }

    protected void testEmptyArray(JsonParser parser) {
        Event e1 = parser.next();
        assertSame(START_ARRAY, e1);
        Event e2 = parser.next();
        assertSame(END_ARRAY, e2);
        assertFalse(parser.hasNext());
    }

}