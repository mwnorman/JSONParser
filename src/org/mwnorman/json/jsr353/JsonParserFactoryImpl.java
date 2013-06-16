package org.mwnorman.json.jsr353;

//javase imports
import java.io.InputStream;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.Map;

//JSR-353 imports
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;

public class JsonParserFactoryImpl implements JsonParserFactory {

    protected JsonProviderImpl jsonProviderImpl;
    protected Map<String, JsonParserConfig> config;

    public JsonParserFactoryImpl(JsonProviderImpl jsonProviderImpl, Map<String, JsonParserConfig> config) {
        this.jsonProviderImpl = jsonProviderImpl;
        this.config = config;
        jsonProviderImpl.setConfig(config);
    }

    @Override
    public JsonParser createParser(Reader reader) {
        return jsonProviderImpl.createParser(reader);
    }

    @Override
    public JsonParser createParser(InputStream in) {
        return jsonProviderImpl.createParser(in);
    }

    @Override
    public JsonParser createParser(InputStream in, Charset charset) {
        return jsonProviderImpl.createParser(in, charset);
    }

    @Override
    public JsonParser createParser(JsonObject obj) {
        return null;
    }

    @Override
    public JsonParser createParser(JsonArray array) {
        return null;
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        return config;
    }

}