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
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.AbstractMap;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

//JSR-353 imports
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonException;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.stream.JsonParser.Event;

//JavaCC-generated imports
import org.mwnorman.json.JSONParser;

class JsonObjectBuilderImpl implements JsonObjectBuilder {

	protected JSONParser jsonParser = null;
	protected final Map<String, JsonValue> valueMap = new LinkedHashMap<String, JsonValue>();

	JsonObjectBuilderImpl() {
	}
	
	JsonObjectBuilderImpl(JSONParser jsonParser) {
		this.jsonParser = jsonParser;
		buildObject();
	}

	//TODO
	private void buildObject() {
        if (jsonParser.hasNext()) {
    		eventLoop:while (jsonParser.hasNext()) {
    			Event e = jsonParser.next();
    			switch (e) {
    				case END_OBJECT:
    					break eventLoop;
    				default:
    					break;
    			}
    		}
        }
        else {
        	throw new JsonException("JSON parsing error: empty iterator");
        }
	}

    public JsonObjectBuilder add(String name, JsonValue value) {
        ensureNotNull(name);
        ensureNotNull(value);
        valueMap.put(name, value);
        return this;
    }

    public JsonObjectBuilder add(String name, String value) {
        ensureNotNull(name);
        ensureNotNull(value);
        valueMap.put(name, new JsonStringImpl(value));
        return this;
    }

    public JsonObjectBuilder add(String name, BigInteger value) {
        ensureNotNull(name);
        ensureNotNull(value);
        valueMap.put(name, new JsonNumberImpl(value));
        return this;
    }

    public JsonObjectBuilder add(String name, BigDecimal value) {
        ensureNotNull(name);
        ensureNotNull(value);
        valueMap.put(name, new JsonNumberImpl(value));
        return this;
    }

    public JsonObjectBuilder add(String name, int value) {
        ensureNotNull(name);
        valueMap.put(name, new JsonNumberImpl(value));
        return this;
    }

    public JsonObjectBuilder add(String name, long value) {
        ensureNotNull(name);
        valueMap.put(name, new JsonNumberImpl(value));
        return this;
    }

    public javax.json.JsonObjectBuilder add(String name, double value) {
        ensureNotNull(name);
        valueMap.put(name, new JsonNumberImpl(value));
        return this;
    }

    public JsonObjectBuilder add(String name, boolean value) {
        ensureNotNull(name);
        valueMap.put(name, value ? JsonValue.TRUE : JsonValue.FALSE);
        return this;
    }

    public JsonObjectBuilder addNull(String name) {
        ensureNotNull(name);
        valueMap.put(name, JsonValue.NULL);
        return this;
    }

    public JsonObjectBuilder add(String name, JsonObjectBuilder builder) {
        ensureNotNull(name);
        if (builder == null) {
            throw new NullPointerException(
                    "Object builder that is used to create a value in JsonObject's name/value pair cannot be null");
        }
        valueMap.put(name, builder.build());
        return this;
    }

    public JsonObjectBuilder add(String name, JsonArrayBuilder builder) {
        ensureNotNull(name);
        if (builder == null) {
            throw new NullPointerException(
                    "Array builder that is used to create a value in JsonObject's name/value pair cannot be null");
        }
        valueMap.put(name, builder.build());
        return this;
    }

    /*
     * don't allow modification to valueMap: wrap it in an unmodifiableMap
     */
	@Override
	public JsonObject build() {
        return new JsonObjectImpl(Collections.unmodifiableMap(valueMap));    
	}

    private void ensureNotNull(Object object) {
        if (object == null) {
            throw new NullPointerException("JsonObject's key/value cannot be null");
        }
    }
	
	static class JsonObjectImpl extends AbstractMap<String, JsonValue> implements JsonObject {

		protected Map<String, JsonValue> valueMap = new LinkedHashMap<String, JsonValue>();
		
		JsonObjectImpl(Map<String, JsonValue> valueMap) {
			this.valueMap = valueMap;
		}

        @Override
        public ValueType getValueType() {
            return ValueType.OBJECT;
        }

        @Override
        public JsonArray getJsonArray(String name) {
            return (JsonArray)get(name);
        }

        @Override
        public JsonObject getJsonObject(String name) {
            return (JsonObject)get(name);
        }

        @Override
        public JsonNumber getJsonNumber(String name) {
            return (JsonNumber)get(name);
        }

        @Override
        public JsonString getJsonString(String name) {
            return (JsonString)get(name);
        }

        @Override
        public String getString(String name) {
            return getJsonString(name).getString();
        }

        @Override
        public String getString(String name, String defaultValue) {
            try {
                return getString(name);
            }
            catch (Exception e) {
                return defaultValue;
            }
        }

        @Override
        public int getInt(String name) {
            return getJsonNumber(name).intValue();
        }

        @Override
        public int getInt(String name, int defaultValue) {
            try {
                return getInt(name);
            }
            catch (Exception e) {
                return defaultValue;
            }
        }

        @Override
        public boolean getBoolean(String name) {
            JsonValue value = valueMap.get(name);
            if (value == null) {
                throw new NullPointerException(String.format("key '%s' not found", name));
            }
            else if (value == JsonValue.TRUE) {
                return true;
            }
            else if (value == JsonValue.FALSE) {
                return false;
            }
            else {
                throw new ClassCastException(String.format("key '%s' not a boolean", name));
            }
        }
		
        @Override
        public boolean getBoolean(String name, boolean defaultValue) {
            try {
                return getBoolean(name);
            }
            catch (Exception e) {
                return defaultValue;
            }
        }

        @Override
        public boolean isNull(String name) {
            return get(name) == JsonValue.NULL;
        }

        //Map APIs
        
		@Override
		public Set<java.util.Map.Entry<String, JsonValue>> entrySet() {
			return valueMap.entrySet();
		}
	}

}