/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.XMLConstants;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;

public class IDS16Ex {
  public Boolean sanitizeByParser;
  public void createXMLStreamBad(final BufferedOutputStream outStream, String quantity) throws IOException {
    String xmlString = getXMLStr();
    if(!sanitizeByParser) {
      if(quantity != null) {
        String badString = xmlString + "<quantity>" + quantity + "</quantity></item>";
        byte[] badData = badString.getBytes();
        outStream.write(badData);  // xmlInput-> end  [IDS16-J]
      } else {
        String goodString = xmlString + "<quantity>" + 300 + "</quantity></item>";
        byte[] goodData = goodString.getBytes();
        outStream.write(goodData);  // xmlInput-> end (goodData is const)
      }
    } else {
      InputSource xmlStream = new InputSource(new StringReader(xmlString));
      // Build a validating SAX parser using our schema
      SchemaFactory sf = SchemaFactory
        .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
      DefaultHandler defHandler = new DefaultHandler() {
        public void warning(SAXParseException s) throws SAXParseException {
          throw s;
        }
        public void error(SAXParseException s) throws SAXParseException {
          throw s;
        }
        public void fatalError(SAXParseException s) throws SAXParseException {
          throw s;
        }
      };
      StreamSource ss = new StreamSource(new File("xmlSchema.xsd"));
      try {
        Schema schema = sf.newSchema(ss);
        SAXParserFactory spf = SAXParserFactory.newInstance();
        spf.setSchema(schema);
        SAXParser saxParser = spf.newSAXParser();
        XMLReader reader = saxParser.getXMLReader();
        reader.setEntityResolver(new CustomResolver());
        saxParser.parse(xmlStream, defHandler);    // xmlInput -> sanitized
      } catch (ParserConfigurationException x) {
        throw new IOException("Unable to validate XML", x);
      } catch (SAXException x) {
        throw new IOException("Invalid XML", x);
      }
      outStream.write(xmlString.getBytes());   // sanitized -> end
    }
    outStream.flush();
  }

  public String getXMLStr()
  {
    String xmlString = "<item>\n<description>Widget</description>\n"
      + "<price>500</price>\n";
    return xmlString;
  }
}

class CustomResolver implements EntityResolver {
  public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException{
    System.out.println("check entity...");
    String entityPath = "correctpath";
    if (systemId.equals(entityPath)){
      System.out.println("leagal："+systemId);
      return new InputSource(entityPath);
    }else{
      System.out.println("illegal"+systemId);
      return new InputSource();
    }
  }
}
