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

import javax.json.JsonString;

class JsonStringImpl implements JsonString {

	private String value;
	
    JsonStringImpl(String value) {
        this.value = value;
    }
    
	@Override
	public ValueType getValueType() {
		return ValueType.STRING;
	}

    @Override
    public String getString() {
        return value;
    }

    @Override
    public CharSequence getChars() {
        return value;
    }

    //need hashCode/equals so that JsonString works properly in JsonObjectImpl which implements Map
    @Override
    public int hashCode() {
        return getString().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof JsonString)) {
            return false;
        }
        JsonString other = (JsonString)obj;
        return getString().equals(other.getString());
    }

}
