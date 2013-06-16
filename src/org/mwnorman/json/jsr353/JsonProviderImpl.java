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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.Map;

//JSR-353 imports
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObjectBuilder;
import javax.json.JsonReader;
import javax.json.JsonReaderFactory;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.spi.JsonProvider;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;

//JavaCC-generated imports
import org.mwnorman.json.JSONParser;
import org.mwnorman.json.JSONParser.JsonLocationImpl;
import org.mwnorman.json.ParseException;
import static org.mwnorman.json.jsr353.JsonParserConfig.CONFIG_PARSER_STRICT;
import static org.mwnorman.json.jsr353.JsonParserUtil.ioExceptionCreatingParser;
import static org.mwnorman.json.jsr353.JsonParserUtil.problemCreatingParserFactoryUnsupportedConfig;
import static org.mwnorman.json.jsr353.JsonParserUtil.parseExceptionCreatingParser;

/**
 * Alternate service provider for JSR-353 {@link javax.json.spi.JsonProvider}
 *
 * @author mwnorman (Mike Norman)
 */
public class JsonProviderImpl extends JsonProvider {

    protected Map<String, JsonParserConfig> config = null;

    @Override
    public JsonParser createParser(Reader reader) {
        JSONParser jsonParser = new JSONParser(reader);
        configParser(jsonParser);
        runParser(jsonParser);
        return jsonParser;
    }

    protected  void configParser(JSONParser jsonParser) {
        if (config != null) {
            JsonParserConfig jsonParserConfig = config.get(CONFIG_PARSER_STRICT);
            if (jsonParserConfig != null) {
                jsonParserConfig.config(jsonParser);
            }
        }
    }

    protected void runParser(JSONParser jsonParser) {
        try {
            jsonParser.parse();
        }
        catch (ParseException pe) {
            if (pe.currentToken != null && pe.currentToken.next != null) {
                throw parseExceptionCreatingParser(pe.getMessage(), pe,
                    new JsonLocationImpl(pe.currentToken.next));
            }
            throw parseExceptionCreatingParser(pe.getMessage(), pe);
        }
    }

    @Override
    public JsonParser createParser(InputStream is) {
        UnicodeWrappingInputStream uwis = null;
        try {
            uwis = new UnicodeWrappingInputStream(is);
        }
        catch (IOException ioe) {
            throw ioExceptionCreatingParser(ioe.getMessage(), ioe);
        }
        catch (ParseException pe) {
            throw parseExceptionCreatingParser(pe.getMessage(), pe);
        }
        JSONParser jsonParser = new JSONParser(uwis, uwis.getCharsetName());
        configParser(jsonParser);
        try {
            jsonParser.parse();
        }
        catch (ParseException pe) {
            if (pe.currentToken != null && pe.currentToken.next != null) {
                throw parseExceptionCreatingParser(pe.getMessage(), pe,
                    new JsonLocationImpl(pe.currentToken.next));
            }
            throw parseExceptionCreatingParser(pe.getMessage(), pe);
        }
        finally {
            try {
                uwis.close();
            }
            catch (IOException e) {
                //ignore
            }
        }
        return jsonParser;
    }

    public JsonParser createParser(InputStream is, Charset charset) {
        JSONParser jsonParser = new JSONParser(is, charset.name());
        configParser(jsonParser);
        try {
            jsonParser.parse();
        }
        catch (ParseException pe) {
            if (pe.currentToken != null && pe.currentToken.next != null) {
                throw parseExceptionCreatingParser(pe.getMessage(), pe,
                    new JsonLocationImpl(pe.currentToken.next));
            }
            throw parseExceptionCreatingParser(pe.getMessage(), pe);
        }
        finally {
            try {
                is.close();
            }
            catch (IOException e) {
                //ignore
            }
        }
        return jsonParser;
    }

    @Override
    public JsonParserFactory createParserFactory(Map<String, ?> config) {
        //my provider supports a number of configs -
        if (config != null) {
            for (Object configValue : config.values()) {
                if (!(configValue instanceof JsonParserConfig)) {
                    throw problemCreatingParserFactoryUnsupportedConfig();
                }
            }
        }
        @SuppressWarnings("unchecked")
        Map<String, JsonParserConfig> jsonParserConfig = (Map<String, JsonParserConfig>)config;
        return new JsonParserFactoryImpl(this, jsonParserConfig);
    }

    @Override
    public JsonGenerator createGenerator(Writer writer) {
        return null;
    }

    @Override
    public JsonGenerator createGenerator(OutputStream out) {
        return null;
    }

    @Override
    public JsonGeneratorFactory createGeneratorFactory(Map<String, ?> config) {
        return null;
    }

    @Override
    public JsonReader createReader(Reader reader) {
        return null;
    }

    @Override
    public JsonReader createReader(InputStream in) {
        return null;
    }

    @Override
    public JsonWriter createWriter(Writer writer) {
        return null;
    }

    @Override
    public JsonWriter createWriter(OutputStream out) {
        return null;
    }

    @Override
    public JsonWriterFactory createWriterFactory(Map<String, ?> config) {
        return null;
    }

    @Override
    public JsonReaderFactory createReaderFactory(Map<String, ?> config) {
        return null;
    }

    @Override
    public JsonObjectBuilder createObjectBuilder() {
        return null;
    }

    @Override
    public JsonArrayBuilder createArrayBuilder() {
        return null;
    }

    @Override
    public JsonBuilderFactory createBuilderFactory(Map<String, ?> config) {
        return null;
    }

    public void setConfig(Map<String, JsonParserConfig> config) {
        this.config  = config;
    }

}