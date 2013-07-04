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

import java.io.InputStream;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.Map;

import javax.json.JsonReader;
import javax.json.JsonReaderFactory;

public class JsonReaderFactoryImpl implements JsonReaderFactory {
	
	protected Map<String, JsonParserConfig> config = Collections.emptyMap();
	
	public JsonReaderFactoryImpl(Map<String, JsonParserConfig> config) {
		this.config = config;
	}

	@Override
	public JsonReader createReader(Reader reader) {
		return new JsonReaderImpl(reader, config);
	}

	@Override
	public JsonReader createReader(InputStream in) {
		return new JsonReaderImpl(in, config);
	}

	@Override
	public JsonReader createReader(InputStream in, Charset charset) {
		return new JsonReaderImpl(in, charset, config);
	}

	@Override
	public Map<String, JsonParserConfig> getConfigInUse() {
		return config;
	}

}