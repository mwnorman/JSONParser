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
package org.mwnorman.json;

//javase imports
import java.io.PrintWriter;
import java.util.List;

//java eXtension JSR-353 imports
import javax.json.JsonValue;
import javax.json.stream.JsonParser.Event;

//my parser imports
import org.mwnorman.json.JSONParser.EventWrapper;

public class JSONParserDisplayer {

    private JSONParser parser;

    public JSONParserDisplayer(JSONParser parser) {
        this.parser = parser;
    }

    static String eventWrapperToString(EventWrapper ew) {
        StringBuilder sb = new StringBuilder();
        switch (ew.event) {
            case END_ARRAY:
                sb.append("]");
                break;
            case END_OBJECT:
                sb.append("}");
                break;
            case KEY_NAME:
                sb.append("\"");
                sb.append(ew.s);
                sb.append("\":");
                break;
            case START_ARRAY:
                sb.append("[");
                break;
            case START_OBJECT:
                sb.append("{");
                break;
            case VALUE_FALSE:
                sb.append(JsonValue.FALSE.toString());
                break;
            case VALUE_NULL:
                sb.append(JsonValue.NULL.toString());
                break;
            case VALUE_NUMBER:
                if (ew.type == EventWrapper.NumType.TYPE_BIGDECIMAL) {
                    sb.append(ew.bd.toString());
                }
                else if (ew.type == EventWrapper.NumType.TYPE_LONG) {
                    sb.append(ew.l);
                }
                else if (ew.type == EventWrapper.NumType.TYPE_INT) {
                    sb.append(ew.i);
                }
                break;
            case VALUE_STRING:
                sb.append("\"");
                sb.append(ew.s);
                sb.append("\"");
                break;
            case VALUE_TRUE:
                sb.append(JsonValue.TRUE.toString());
                break;
            default:
                break;
        }
        return sb.toString();
    }

    public void display(PrintWriter pw) {
        List<EventWrapper> stack = parser.getStack();
        for (int i = 0, len = stack.size(); i < len; i++) {
            EventWrapper ew = stack.get(i);
            pw.print(eventWrapperToString(ew));
            if (i < len-1) {
                EventWrapper nextew = stack.get(i+1);
                if (!(nextew.event == Event.END_ARRAY ||
                    nextew.event == Event.END_OBJECT ||
                    nextew.event == Event.START_OBJECT ||
                    nextew.event == Event.START_ARRAY ||
                    nextew.event == Event.KEY_NAME)) {
                    pw.print(" ");
                }
                else if (ew.event == Event.VALUE_STRING && nextew.event == Event.KEY_NAME) {
                    pw.print(", ");
                }
                else if (ew.event == Event.VALUE_NUMBER  && nextew.event == Event.KEY_NAME) {
                    pw.print(", ");
                }
                else if ((ew.event == Event.END_OBJECT || ew.event == Event.END_ARRAY)  &&
                    (nextew.event == Event.START_OBJECT || nextew.event == Event.START_ARRAY)) {
                    pw.print(",");
                }
            }
        }
        pw.flush();
    }
}