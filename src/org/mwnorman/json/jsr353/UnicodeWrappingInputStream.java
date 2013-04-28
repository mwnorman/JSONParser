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
import java.io.PushbackInputStream;
import java.nio.charset.StandardCharsets;

import org.mwnorman.json.ParseException;

/**
 * The <code>UnicodeWrapperInputStream</code> class wraps the specified
 * <code>InputStream</code> and deduces what Unicode encoding it uses.
 *
 */
public class UnicodeWrappingInputStream extends InputStream {

    //there are many encodings and code-points, but what
    //we are really interested in is character width:
    //1, 2 or 4 byte characters
    static enum CHARSET {
         UNKNOWN("UNKNOWN")
        ,UTF_8(StandardCharsets.UTF_8.name())
        ,UTF_16LE(StandardCharsets.UTF_16LE.name())
        ,UTF_16BE(StandardCharsets.UTF_16BE.name())
        ,UTF_32LE("UTF-32LE")  /* no StandardCharsets constants for UTF-32 charsets :-( */
        ,UTF_32BE("UTF-32BE");
        private String charsetName;
        private CHARSET(String charsetName) {
             this.charsetName = charsetName;
        }
        public String getCharsetName() {
            return charsetName;
        }
    }

    private static final int BOM_SIZE = 4;

    protected PushbackInputStream pushbackStream = null;
    protected CHARSET charset = CHARSET.UNKNOWN;

    public UnicodeWrappingInputStream(InputStream is) throws IOException, ParseException {
        if (is == null) {
            throw new NullPointerException("null input stream not allowed");
        }
        // try to detect BOM:
        byte[] buf = new byte[BOM_SIZE];
        //use PushbackInputStream 'cause quite often there is no BOM so we
        //have to give back the (up to) four characters we've already read
        pushbackStream = new PushbackInputStream(is, BOM_SIZE);
        int actuallyRead = pushbackStream.read(buf, 0, BOM_SIZE);
        int unread = actuallyRead;
        if (actuallyRead < 2) {
            throw new ParseException("Cannot parse InputStream: " +
        		"not enough data to determine Unicode Charset, nor to be a valid JSON doc");
        }
        else {
            /* http://www.unicode.org/unicode/faq/utf_bom.html
                   00 00 FE FF    = UTF-32, big-endian
                   FF FE 00 00    = UTF-32, little-endian
                   EF BB BF       = UTF-8,
                   FE FF          = UTF-16, big-endian
                   FF FE          = UTF-16, little-endian
            */
            if (buf[0] == (byte)0x00 && buf[1] == (byte)0x00 && buf[2] == (byte)0xfe && buf[3] == (byte)0xff) {
                unread = 0;
                charset = CHARSET.UTF_32BE;
            }
            else if (buf[0] == (byte)0xff && buf[1] == (byte)0xfe && buf[2] == (byte)0x00 && buf[3] == (byte)0x00) {
                unread = 0;
                charset = CHARSET.UTF_32LE;
            }
            else if (buf[0] == (byte)0xfe && buf[1] == (byte)0xff) {
                unread = 2;
                charset = CHARSET.UTF_16BE;
            }
            else if (buf[0] == (byte)0xff && buf[1] == (byte)0xfe) {
                unread = 2;
                charset = CHARSET.UTF_16LE;
            }
            else if (buf[0] == (byte)0xef && buf[1] == (byte)0xbb && buf[2] == (byte)0xbf) {
                unread = 1;
                charset = CHARSET.UTF_8;
            }
            else {
                // No BOM found - detect NULs by character width
                if (buf[0] == (byte)0x00 && buf[1] == (byte)0x00 && buf[2] == (byte)0x00) {
                    charset = CHARSET.UTF_32BE;
                }
                else if (buf[0] == (byte)0x00 && buf[2] == (byte)0x00) {
                    charset = CHARSET.UTF_16BE;
                }
                else if (buf[1] == (byte)0x00 && buf[2] == (byte)0x00 && buf[3] == (byte)0x00) {
                    charset = CHARSET.UTF_32LE;
                }
                else if (buf[1] == (byte)0x00 && buf[3] == (byte)0x00) {
                    charset = CHARSET.UTF_16LE;
                }
            }
        }
        // Unread bytes, skip BOM marks.
        if (unread > 0) {
            pushbackStream.unread(buf, (actuallyRead - unread), unread);
        }
    }

    public String getCharsetName() {
        if (charset == CHARSET.UNKNOWN) {
            return null;
        }
        return charset.getCharsetName();
    }

    @Override
    public int read(byte[] b) throws IOException {
        return pushbackStream.read(b);
    }

    @Override
    public long skip(long n) throws IOException {
        return pushbackStream.skip(n);
    }

    @Override
    public int available() throws IOException {
        return pushbackStream.available();
    }

    @Override
    public synchronized void mark(int readlimit) {
        pushbackStream.mark(readlimit);
    }

    @Override
    public synchronized void reset() throws IOException {
        pushbackStream.reset();
    }

    @Override
    public boolean markSupported() {
        return pushbackStream.markSupported();
    }

    @Override
    public int read() throws IOException {
        return pushbackStream.read();
    }

    @Override
    public int read(byte b[], int off, int len) throws IOException {
        return pushbackStream.read(b, off, len);
    }

    @Override
    public void close() throws IOException {
        pushbackStream.close();
    }
}