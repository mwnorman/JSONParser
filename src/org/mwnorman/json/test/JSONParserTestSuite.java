package org.mwnorman.json.test;

//javase imports
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

//JUnit4 imports
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;

//domain-specific imports
import org.mwnorman.json.JSONParser;
import org.mwnorman.json.ParseException;

@SuppressWarnings("rawtypes")
public class JSONParserTestSuite {

	static final String DOLLAR_SIGN = "$";
	static final String QUOTE = "\"";
	static final String OPENBRACE = "{";
	static final String CLOSEBRACE = "}";
	static final String DOUBLE_SLASH = "\\\\";
	static final String KEY = "key";
	static final String VALUE = "value";

	@Test
	public void trueAtom() {
		StringReader stringReader = new StringReader("[true]");
		JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Object parsedJSON = null;
        try {
        	parsedJSON = parser.parse();
		} catch (ParseException e) {
			worked = false;
		}
        assertTrue(worked);
        assertNotNull(parsedJSON);
        @SuppressWarnings("unchecked")ArrayList<Object> a = (ArrayList<Object>)parsedJSON;
        Object atom = a.get(0);
        assertNotNull(atom);
        assertSame(Boolean.TRUE, atom);
    }
	
	@Test
	public void falseAtom() {
		StringReader stringReader = new StringReader("[false]");
		JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Object parsedJSON = null;
        try {
        	parsedJSON = parser.parse();
		} catch (ParseException e) {
			worked = false;
		}
        assertTrue(worked);
        assertNotNull(parsedJSON);
        @SuppressWarnings("unchecked")ArrayList<Object> a = (ArrayList<Object>)parsedJSON;
        Object atom = a.get(0);
        assertNotNull(atom);
        assertSame(Boolean.FALSE, atom);
    }
	
	@Test
	public void numerAtom() {
		StringReader stringReader = new StringReader("[42]");
		JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Object parsedJSON = null;
        try {
        	parsedJSON = parser.parse();
		} catch (ParseException e) {
			worked = false;
		}
        assertTrue(worked);
        assertNotNull(parsedJSON);
        @SuppressWarnings("unchecked")ArrayList<Object> a = (ArrayList<Object>)parsedJSON;
        Object atom = a.get(0);
        assertNotNull(atom);
        assertEquals(42, atom);
    }
	
	static final String NO_DOUBLE_SLASH_IN_KEY =
		OPENBRACE +
			QUOTE + KEY + QUOTE + ":" + QUOTE + VALUE + QUOTE +
		CLOSEBRACE;
    @Test
    public void noDoubleSlashAtEnd() {
        StringReader stringReader = new StringReader(NO_DOUBLE_SLASH_IN_KEY);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Object parsedJSON = null;
        try {
        	parsedJSON = parser.parse();
		} catch (ParseException e) {
			worked = false;
		}
        assertTrue(worked);
        assertNotNull(parsedJSON);
    }

	static final String DOUBLE_SLASH_IN_KEY =
		OPENBRACE +
			QUOTE + KEY + DOUBLE_SLASH + QUOTE + ":" + QUOTE + VALUE + QUOTE +
		CLOSEBRACE;
	@Test
    public void doubleSlashAtEnd() {
            StringReader stringReader = new StringReader(DOUBLE_SLASH_IN_KEY);
            JSONParser parser = new JSONParser(stringReader);
            boolean worked = true;
            Object parsedJSON = null;
            try {
            	parsedJSON = parser.parse();
    		} catch (ParseException e) {
    			worked = false;
    		}
            assertTrue(worked);
            assertNotNull(parsedJSON);
            String key = (String)((Map)parsedJSON).keySet().iterator().next();
            assertTrue(key.endsWith(DOUBLE_SLASH));
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
            Object parsedJSON = null;
            try {
            	parsedJSON = parser.parse();
    		} catch (ParseException e) {
    			worked = false;
    		}
            assertTrue(worked);
            assertNotNull(parsedJSON);
            Map attrsMap = (Map)parsedJSON;
            Map innerMap = (Map) attrsMap.get(ATTRS);
            String dollarAllKey = (String)innerMap.keySet().iterator().next();
            assertTrue(dollarAllKey.startsWith(DOLLAR_SIGN));
    }

    static final String NESTED_ARRAY =
    	"[0,{\"1\":{\"2\":{\"3\":{\"4\":[5,{\"6\":7}]}}}}]";
    @Test
    public void nestedMaps() {
        StringReader stringReader = new StringReader(NESTED_ARRAY);
        JSONParser parser = new JSONParser(stringReader);
        boolean worked = true;
        Object parsedJSON = null;
        try {
        	parsedJSON = parser.parse();
		} catch (ParseException e) {
			worked = false;
		}
        assertTrue(worked);
        assertNotNull(parsedJSON);
        Integer firstValue = (Integer)((List)parsedJSON).get(0);
        assertEquals(0L, firstValue.longValue());
        Map firstMap = (Map)((List)parsedJSON).get(1);
        assertNotNull(firstMap);
        Set firstKeys = firstMap.keySet();
        assertTrue(firstKeys.size() == 1);
        String firstKey = (String) firstKeys.iterator().next();
        assertEquals("1", firstKey);
    }
/*        
        Object obj=JSONValue.parse(s);
        JSONArray array=(JSONArray)obj;
        System.out.println("======the 2nd element of array======");
        System.out.println();
        assertEquals("{\"1\":{\"2\":{\"3\":{\"4\":[5,{\"6\":7}]}}}}",array.get(1).toString());
        
        JSONObject obj2=(JSONObject)array.get(1);
        System.out.println("======field \"1\"==========");
        System.out.println(obj2.get("1"));      
        assertEquals("{\"2\":{\"3\":{\"4\":[5,{\"6\":7}]}}}",obj2.get("1").toString());
        
        s="{}";
        obj=JSONValue.parse(s);
        assertEquals("{}",obj.toString());
        
        s="[5,]";
        obj=JSONValue.parse(s);
        assertEquals("[5]",obj.toString());
        
        s="[5,,2]";
        obj=JSONValue.parse(s);
        assertEquals("[5,2]",obj.toString());
        
        s="[\"hello\\bworld\\\"abc\\tdef\\\\ghi\\rjkl\\n123\\u4e2d\"]";
        obj=JSONValue.parse(s);
        assertEquals("hello\bworld\"abc\tdef\\ghi\rjkl\n123中",((List)obj).get(0).toString());
        
        JSONParser parser = new JSONParser();
        s="{\"name\":";
        try{
                obj = parser.parse(s);
        }
        catch(ParseException pe){
                assertEquals(ParseException.ERROR_UNEXPECTED_TOKEN, pe.getErrorType());
                assertEquals(8, pe.getPosition());
        }
        
        s="{\"name\":}";
        try{
                obj = parser.parse(s);
        }
        catch(ParseException pe){
                assertEquals(ParseException.ERROR_UNEXPECTED_TOKEN, pe.getErrorType());
                assertEquals(8, pe.getPosition());
        }
        
        
        s="{\"name";
        try{
                obj = parser.parse(s);
        }
        catch(ParseException pe){
                assertEquals(ParseException.ERROR_UNEXPECTED_TOKEN, pe.getErrorType());
                assertEquals(6, pe.getPosition());
        }
        
        
        s = "[[null, 123.45, \"a\\\tb c\"}, true]";
        try{
                parser.parse(s);
        }
        catch(ParseException pe){
                assertEquals(24, pe.getPosition());
                System.out.println("Error at character position: " + pe.getPosition());
                switch(pe.getErrorType()){
                case ParseException.ERROR_UNEXPECTED_TOKEN:
                        System.out.println("Unexpected token: " + pe.getUnexpectedObject());
                        break;
                case ParseException.ERROR_UNEXPECTED_CHAR:
                        System.out.println("Unexpected character: " + pe.getUnexpectedObject());
                        break;
                case ParseException.ERROR_UNEXPECTED_EXCEPTION:
                        ((Exception)pe.getUnexpectedObject()).printStackTrace();
                        break;
                }
        }
        
        s = "{\"first\": 123, \"second\": [4, 5, 6], \"third\": 789}";
        ContainerFactory containerFactory = new ContainerFactory(){
                public List creatArrayContainer() {
                        return new LinkedList();
                }

                public Map createObjectContainer() {
                        return new LinkedHashMap();
                }
                
        };
        
        try{
                Map json = (Map)parser.parse(s, containerFactory);
                Iterator iter = json.entrySet().iterator();
                System.out.println("==iterate result==");
                while(iter.hasNext()){
                        Map.Entry entry = (Map.Entry)iter.next();
                        System.out.println(entry.getKey() + "=>" + entry.getValue());
                }
                
                System.out.println("==toJSONString()==");                       
                System.out.println(JSONValue.toJSONString(json));
                assertEquals("{\"first\":123,\"second\":[4,5,6],\"third\":789}", JSONValue.toJSONString(json));
        }
        catch(ParseException pe){
                pe.printStackTrace();
        }
        
        s = "{\"first\": 123, \"second\": [{\"s1\":{\"s11\":\"v11\"}}, 4, 5, 6], \"third\": 789}";
        ContentHandler myHandler = new ContentHandler() {

                public boolean endArray() throws ParseException {
                        System.out.println("endArray()");
                        return true;
                }

                public void endJSON() throws ParseException {
                        System.out.println("endJSON()");
                }

                public boolean endObject() throws ParseException {
                        System.out.println("endObject()");
                        return true;
                }

                public boolean endObjectEntry() throws ParseException {
                        System.out.println("endObjectEntry()");
                        return true;
                }

                public boolean primitive(Object value) throws ParseException {
                        System.out.println("primitive(): " + value);
                        return true;
                }

                public boolean startArray() throws ParseException {
                        System.out.println("startArray()");
                        return true;
                }

                public void startJSON() throws ParseException {
                        System.out.println("startJSON()");
                }

                public boolean startObject() throws ParseException {
                        System.out.println("startObject()");
                        return true;
                }

                public boolean startObjectEntry(String key) throws ParseException {
                        System.out.println("startObjectEntry(), key:" + key);
                        return true;
                }
                
        };
        try{
                parser.parse(s, myHandler);
        }
        catch(ParseException pe){
                pe.printStackTrace();
        }

class KeyFinder implements ContentHandler{
    private Object value;
    private boolean found = false;
    private boolean end = false;
    private String key;
    private String matchKey;
    
    public void setMatchKey(String matchKey){
        this.matchKey = matchKey;
    }
    
    public Object getValue(){
        return value;
    }
    
    public boolean isEnd(){
        return end;
    }
    
    public void setFound(boolean found){
        this.found = found;
    }
    
    public boolean isFound(){
        return found;
    }
    
    public void startJSON() throws ParseException, IOException {
        found = false;
        end = false;
    }

    public void endJSON() throws ParseException, IOException {
        end = true;
    }

    public boolean primitive(Object value) throws ParseException, IOException {
        if(key != null){
            if(key.equals(matchKey)){
                found = true;
                this.value = value;
                key = null;
                return false;
            }
        }
        return true;
    }

    public boolean startArray() throws ParseException, IOException {
        return true;
    }

    
    public boolean startObject() throws ParseException, IOException {
        return true;
    }

    public boolean startObjectEntry(String key) throws ParseException, IOException {
        this.key = key;
        return true;
    }
    
    public boolean endArray() throws ParseException, IOException {
        return false;
    }

    public boolean endObject() throws ParseException, IOException {
        return true;
    }

    public boolean endObjectEntry() throws ParseException, IOException {
        return true;
    }
};

s = "{\"first\": 123, \"second\": [{\"k1\":{\"id\":\"id1\"}}, 4, 5, 6, {\"id\": 123}], \"third\": 789, \"id\": null}";
parser.reset();
KeyFinder keyFinder = new KeyFinder();
keyFinder.setMatchKey("id");
int i = 0;
try{
    while(!keyFinder.isEnd()){
        parser.parse(s, keyFinder, true);
        if(keyFinder.isFound()){
            i++;
            keyFinder.setFound(false);
            System.out.println("found id:");
            System.out.println(keyFinder.getValue());
            if(i == 1)
                assertEquals("id1", keyFinder.getValue());
            if(i == 2){
                assertTrue(keyFinder.getValue() instanceof Number);
                assertEquals("123", String.valueOf(keyFinder.getValue()));
            }
            if(i == 3)
                assertTrue(null == keyFinder.getValue());
        }
    }
}
catch(ParseException pe){
    pe.printStackTrace();
}
}
*/
}
