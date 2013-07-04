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
import java.text.MessageFormat;

//JSR-353 imports
import javax.json.JsonException;
import javax.json.stream.JsonLocation;
import javax.json.stream.JsonParsingException;

//JavaCC-generated imports
import org.mwnorman.json.ParseException;
import static org.mwnorman.json.JSONParser.NULL_LOCATION;

public final class JsonParserUtil {

    static JsonParserConfig STRICT_PARSER_CONFIG = new StrictParserConfig();

    public final static JsonParserConfig getStrictConfig() {
        return STRICT_PARSER_CONFIG;
    }

    public static final String PARSE_EXCEPTION_CREATING_PARSER =
        "Parse exception occurred trying to create a Json Parser.\nException message: {0}";

    public static final String IO_EXCEPTION_CREATING_PARSER =
        "I/O exception occurred trying to create a Json Parser.\nException message: {0}";

    public static final String PROBLEM_CREATING_PARSER_FACTORY_UNSUPPORTED_CONFIG =
        "Problem creating JsonParserFactory: Unsupported config provided";

    public static JsonParsingException parseExceptionCreatingParser(String exceptionMessage,
        ParseException parseException) {
        return parseExceptionCreatingParser(exceptionMessage, parseException, NULL_LOCATION);
    }

    public static JsonParsingException parseExceptionCreatingParser(String exceptionMessage,
        ParseException parseException, JsonLocation jsonLocation) {
        return new JsonParsingException(
            MessageFormat.format(PARSE_EXCEPTION_CREATING_PARSER, exceptionMessage),
            parseException, jsonLocation);
    }

    public static JsonParsingException ioExceptionCreatingParser(String exceptionMessage,
        IOException ioException) {
        return new JsonParsingException(
            MessageFormat.format(IO_EXCEPTION_CREATING_PARSER, exceptionMessage),
            ioException, NULL_LOCATION);
    }

    public static JsonException problemCreatingParserFactoryUnsupportedConfig() {
        return new JsonException(PROBLEM_CREATING_PARSER_FACTORY_UNSUPPORTED_CONFIG);
    }

}