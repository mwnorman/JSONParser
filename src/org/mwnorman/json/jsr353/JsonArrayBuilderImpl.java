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
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


//JSR-353 imports
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.stream.JsonParser.Event;


//JavaCC-generated imports
import org.mwnorman.json.JSONParser;

class JsonArrayBuilderImpl implements JsonArrayBuilder {

	private JSONParser jsonParser = null;
	private List<JsonValue> valueList = new ArrayList<JsonValue>();

	JsonArrayBuilderImpl() {
	}
	
	JsonArrayBuilderImpl(JSONParser jsonParser) {
		this.jsonParser = jsonParser;
		buildArray();
	}

	//TODO
	private void buildArray() {
		eventLoop:while (jsonParser.hasNext()) {
			Event e = jsonParser.next();
			switch (e) {
				case END_ARRAY:
					break eventLoop;
				default:
					break;
			}
		}
	}

    public JsonArrayBuilder add(JsonValue value) {
    	ensureNotNull(value);
        valueList.add(value);
        return this;
    }

    public JsonArrayBuilder add(String value) {
    	ensureNotNull(value);
        valueList.add(new JsonStringImpl(value));
        return this;
    }

    public JsonArrayBuilder add(BigDecimal value) {
    	ensureNotNull(value);
        valueList.add(new JsonNumberImpl(value));
        return this;
    }

    public JsonArrayBuilder add(BigInteger value) {
    	ensureNotNull(value);
        valueList.add(new JsonNumberImpl(value));
        return this;
    }

    public JsonArrayBuilder add(int value) {
        valueList.add(new JsonNumberImpl(value));
        return this;
    }

    public JsonArrayBuilder add(long value) {
        valueList.add(new JsonNumberImpl(value));
        return this;
    }

    public JsonArrayBuilder add(double value) {
        valueList.add(new JsonNumberImpl(value));
        return this;
    }

    public JsonArrayBuilder add(boolean value) {
        valueList.add(value ? JsonValue.TRUE : JsonValue.FALSE);
        return this;
    }

    public JsonArrayBuilder addNull() {
        valueList.add(JsonValue.NULL);
        return this;
    }

    public JsonArrayBuilder add(JsonObjectBuilder builder) {
        if (builder == null) {
            throw new NullPointerException("Object builder that is used to add a value to JSON array cannot be null");
        }
        valueList.add(builder.build());
        return this;
    }

    public JsonArrayBuilder add(JsonArrayBuilder builder) {
        if (builder == null) {
            throw new NullPointerException("Array builder that is used to add a value to JSON array cannot be null");
        }
        valueList.add(builder.build());
        return this;
    }

    /*
     * don't allow modification to valueMap: wrap it in an unmodifiableMap
     */
	@Override
	public JsonArray build() {
        return new JsonArrayImpl(Collections.unmodifiableList(valueList));    
	}

    private void ensureNotNull(Object object) {
        if (object == null) {
            throw new NullPointerException("JsonArray's value cannot be null");
        }
    }
	
	static class JsonArrayImpl extends AbstractList<JsonValue> implements JsonArray {
        private List<JsonValue> valueList;
        JsonArrayImpl(List<JsonValue> valueList) {
            this.valueList = valueList;
        }
		@Override
		public ValueType getValueType() {
			return ValueType.ARRAY;
		}
        @Override
        public JsonObject getJsonObject(int index) {
            return (JsonObject)valueList.get(index);
        }
        @Override
        public JsonArray getJsonArray(int index) {
            return (JsonArray)valueList.get(index);
        }
        @Override
        public JsonNumber getJsonNumber(int index) {
            return (JsonNumber)valueList.get(index);
        }
        @Override
        public JsonString getJsonString(int index) {
            return (JsonString)valueList.get(index);
        }
        @Override
        @SuppressWarnings("unchecked")
        public <T extends JsonValue> List<T> getValuesAs(Class<T> clazz) {
            return (List<T>)valueList;
        }
        @Override
        public String getString(int index) {
            return getJsonString(index).getString();
        }
        @Override
        public String getString(int index, String defaultValue) {
            try {
                return getString(index);
            }
            catch (Exception e) {
                return defaultValue;
            }
        }
        @Override
        public int getInt(int index) {
            return getJsonNumber(index).intValue();
        }
        @Override
        public int getInt(int index, int defaultValue) {
            try {
                return getInt(index);
            }
            catch (Exception e) {
                return defaultValue;
            }
        }
        @Override
        public boolean getBoolean(int index) {
            JsonValue jsonValue = get(index);
            if (jsonValue == JsonValue.TRUE) {
                return true;
            }
            else if (jsonValue == JsonValue.FALSE) {
                return false;
            }
            else {
                throw new ClassCastException(String.format("value at index %d not a boolean", index));
            }
        }
        @Override
        public boolean getBoolean(int index, boolean defaultValue) {
            try {
                return getBoolean(index);
            }
            catch (Exception e) {
                return defaultValue;
            }
        }
		@Override
		public boolean isNull(int index) {
            return get(index) == JsonValue.NULL;
		}

		//List APIs
		@Override
		public JsonValue get(int index) {
			return valueList.get(index);
		}
		@Override
		public int size() {
			return valueList.size();
		}
	}

}