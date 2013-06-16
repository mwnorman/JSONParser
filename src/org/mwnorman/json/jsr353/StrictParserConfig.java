package org.mwnorman.json.jsr353;

import org.mwnorman.json.JSONParser;

public class StrictParserConfig implements JsonParserConfig {

    @Override
    public void config(JSONParser jsonParser) {
        jsonParser.setStrict();
    }

}