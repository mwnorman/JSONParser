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
package org.mwnorman.json.jsr353;

//javase imports
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Map;

//JSR-353 imports
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonReader;
import javax.json.JsonStructure;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParser.Event;
import static javax.json.stream.JsonParser.Event.START_ARRAY;
import static javax.json.stream.JsonParser.Event.START_OBJECT;

//JavaCC-generated imports
import org.mwnorman.json.JSONParser;

//My JSR-353 impls

public class JsonReaderImpl implements JsonReader {

	protected Map<String, JsonParserConfig> config = null;
	protected JsonProviderImpl jsonProvider = null;
	protected JSONParser jsonParser = null;

	public JsonReaderImpl(Reader reader, Map<String, JsonParserConfig> config) {
		this(config);
		jsonParser = (JSONParser)jsonProvider.createParser(reader);
	}

	public JsonReaderImpl(InputStream in, Map<String, JsonParserConfig> config) {
		this(config);
		jsonParser = (JSONParser)jsonProvider.createParser(in);
	}

	public JsonReaderImpl(InputStream in, Charset charset, Map<String, JsonParserConfig> config) {
		this(config);
		jsonParser = (JSONParser)jsonProvider.createParser(in, charset);
	}

	protected JsonReaderImpl(Map<String, JsonParserConfig> config) {
		this();
		this.config = config;
		jsonProvider.setConfig(config);
	}
	
	protected JsonReaderImpl() {
		jsonProvider = new JsonProviderImpl();
	}

	@Override
	public JsonStructure read() {
		JsonStructure jsonStructure = null;
        if (jsonParser.hasNext()) {
            Event e = jsonParser.next();
            switch (e) {
    			case START_ARRAY:
    				jsonStructure = readArray();
    				break;
    			case START_OBJECT :
    				jsonStructure = readObject();
    				break;
    			default:
                    throw new JsonException(String.format("JSON parsing error: no START event (parsing event = %s",e));
            }
        }
        else {
        	throw new JsonException("JSON parsing error: empty iterator");
        }
        return jsonStructure;
	}

	@Override
	public JsonObject readObject() {
		JsonObject jsonObject = null;
        if (jsonParser.hasNext()) {
            Event e = jsonParser.next();
            if (e == START_OBJECT) {
            	jsonObject = readObject(new JsonObjectBuilderImpl());
            }
            else {
                throw new JsonException(String.format("JSON parsing error: no START_OBJECT event (parsing event = %s",e));
            }
        }
        else {
        	throw new JsonException("JSON parsing error: empty iterator");
        }
        return jsonObject;
	}
	
	protected JsonObject readObject(JsonObjectBuilder builder) {
        String key = null;
        while (jsonParser.hasNext()) {
            JsonParser.Event e = jsonParser.next();
            switch (e) {
                case START_ARRAY:
                    JsonArray array = readArray(new JsonArrayBuilderImpl());
                    builder.add(key, array);
                    break;
                case START_OBJECT:
                    JsonObject object = readObject(new JsonObjectBuilderImpl());
                    builder.add(key, object);
                    break;
                case KEY_NAME:
                    key = jsonParser.getString();
                    break;
                case VALUE_STRING:
                    String  string = jsonParser.getString();
                    builder.add(key, string);
                    break;
                case VALUE_NUMBER:
                    BigDecimal bd = new BigDecimal(jsonParser.getString());
                    builder.add(key, bd);
                    break;
                case VALUE_TRUE:
                    builder.add(key, true);
                    break;
                case VALUE_FALSE:
                    builder.add(key, false);
                    break;
                case VALUE_NULL:
                    builder.addNull(key);
                    break;
                case END_OBJECT:
                    return builder.build();
                default:
                    throw new JsonException("Internal Error");
            }
        }
        throw new JsonException("Internal Error");
    }

	@Override
	public JsonArray readArray() {
		JsonArray jsonArray = null;
        if (jsonParser.hasNext()) {
            Event e = jsonParser.next();
            if (e == START_ARRAY) {
                jsonArray = readArray(new JsonArrayBuilderImpl());
            }
            else {
                throw new JsonException(String.format("JSON parsing error: no START_ARRAY event (parsing event = %s",e));
            }
        }
        else {
        	throw new JsonException("JSON parsing error: empty iterator");
        }
        return jsonArray;
	}
	
	protected JsonArray readArray(JsonArrayBuilder builder) {
        while (jsonParser.hasNext()) {
            JsonParser.Event e = jsonParser.next();
            switch (e) {
                case START_ARRAY:
                    JsonArray array = readArray(new JsonArrayBuilderImpl());
                    builder.add(array);
                    break;
                case START_OBJECT:
                    JsonObject object = readObject(new JsonObjectBuilderImpl());
                    builder.add(object);
                    break;
                case VALUE_STRING:
                    String  string = jsonParser.getString();
                    builder.add(string);
                    break;
                case VALUE_NUMBER:
                    BigDecimal bd = new BigDecimal(jsonParser.getString());
                    builder.add(bd);
                    break;
                case VALUE_TRUE:
                    builder.add(true);
                    break;
                case VALUE_FALSE:
                    builder.add(false);
                    break;
                case VALUE_NULL:
                    builder.addNull();
                    break;
                case END_ARRAY:
                    return builder.build();
                default:
                    throw new JsonException("Internal Error");
            }
        }
        throw new JsonException("Internal Error");
    }

	@Override
	public void close() {
		jsonParser.close();
	}

}