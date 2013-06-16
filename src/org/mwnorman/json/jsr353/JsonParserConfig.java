package org.mwnorman.json.jsr353;

import org.mwnorman.json.JSONParser;

public interface JsonParserConfig {

    public static final String CONFIG_PARSER_STRICT = "config.parser.strict";

    public void config(JSONParser jsonParser);

}