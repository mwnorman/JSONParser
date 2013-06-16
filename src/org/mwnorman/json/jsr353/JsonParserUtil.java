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