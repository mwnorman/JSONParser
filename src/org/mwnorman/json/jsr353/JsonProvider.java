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
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Map;

//JSR-353 imports
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObjectBuilder;
import javax.json.JsonReader;
import javax.json.JsonReaderFactory;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;
import javax.json.stream.JsonLocation;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;
import javax.json.stream.JsonParsingException;

//JavaCC-generated imports
import org.mwnorman.json.JSONParser;
import org.mwnorman.json.JSONParser.JsonLocationImpl;
import org.mwnorman.json.ParseException;
import static org.mwnorman.json.JSONParser.NULL_LOCATION;

/**
 * Alternate service provider for JSR-353 {@link javax.json.spi.JsonProvider}
 *
 * @author mwnorman (Mike Norman)
 */
public class JsonProvider extends javax.json.spi.JsonProvider {

    @Override
    public JsonParser createParser(Reader reader) {
        JSONParser parser = new JSONParser(reader);
        try {
            parser.parse();
        }
        catch (ParseException pe) {
            JsonLocation location = NULL_LOCATION;
            if (pe.currentToken != null && pe.currentToken.next != null) {
                location = new JsonLocationImpl(pe.currentToken.next);
            }
            throw new JsonParsingException(pe.getMessage(), pe, location);
        }
        return parser;
    }

    @Override
    public JsonParser createParser(InputStream in) {
        JSONParser parser = new JSONParser(in);
        try {
            parser.parse();
        }
        catch (ParseException pe) {
            JsonLocation location = NULL_LOCATION;
            if (pe.currentToken != null && pe.currentToken.next != null) {
                location = new JsonLocationImpl(pe.currentToken.next);
            }
            throw new JsonParsingException(pe.getMessage(), pe, location);
        }
        return parser;
    }

    @Override
    public JsonParserFactory createParserFactory(Map<String, ?> config) {
        return null;
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

}