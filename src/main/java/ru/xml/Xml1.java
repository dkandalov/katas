package xml;

import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.FileReader;
import java.util.Set;
import java.util.TreeSet;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: 17/4/11
 */
public class Xml1 {
	private static final String FILENAME = "/Users/dima/IdeaProjects/katas_new/java/src/cmc/template/resources/orders2.xml";

	@Test public void shouldCreateSetOfSymbols() throws Exception {
		Set<String> symbols = readSymbols_SAX(FILENAME);
		assertThat(symbols.toString(), equalTo(
				"[ANLLBXHEAP, BU_G_QMCGJ, BYZDFBCKMU, CMNWVCWGCJ, CPJGFTJSYU, CTSYSZKXNO, DEYIUAWWUE, " +
						"DINGKS_NCN, DKH__IEBXV, EGYYUCMJUI, FBSTRYGBXF, FICQNLGWPM, FJMCVT_OPD, FMAHIAPPQI, " +
						"FOADKAYSJI, FOSCTMKOYL, GBXXBVSXTE, GSXANG_S_P, HMI_GOOWYZ, HPETUXGUAP, HTQGBIGZ_M, " +
						"H_ALDOUCOP, IAJMUUCNDO, IEUXHYLXQT, IITGRMTWSC, IQCOPHWXKR, IQRAIH_NAR, IQVTCXOSLQ, " +
						"JAPZWTPXAG, JBMLYFYYFP, JCOFOSLLDO, JF_LIUJYWU, JGWHRUYZXV, JO_IBKRVFN, JUVMGSDDNR, " +
						"JYLCJK_GRQ, KKSMUZFPIT, KRPJ__MYON, K_EZNFAWWZ, LLAIXANEGQ, LSHYSRKVJF, LSZPDHAOLG, " +
						"L_ZZERPXGH, NMBZTDSUBR, NNSLTBEBBJ, NSMDDUCCIU, NXPAHJTONP, NXQJUX__MH, OBHIKNROPS, " +
						"OGEHASKBTR, OGHHPFFKC_, OHJKLGNHQO, OJY_XSNSVH, OLTVPI_YMT, OOWDMWZQDU, OUKQJMFENY, " +
						"OURCUJB_Q_, PBAOZCEXQM, PEXXHCROCO, PZWI_HGNXI, QJKFHEOSLX, QZA_WGDAAN, RD___TUVWR, " +
						"RIOAGVNSVL, RJCQLLXBYN, SHSNCLEZCT, SXDVWFPVVN, TBRJSIJJXR, TQAARLCKGW, TQGYGKCDKN, " +
						"UIDPAKLH_R, UJJIZJLXHX, UNG_GVXECY, UOVPGCNLTQ, USV_LUW_AB, VAGFLEAXWG, VWLYBWQQTO, " +
						"VZLPDRSRXJ, WHTZHIWCAI, WS_QDJQERN, WXJHTHLOVY, WYEUL_HEAM, XHEQOZYQKF, XIKFJDHEQH, " +
						"XUNJTRJWPA, XWLUMUYAWV, YAYMXEXLEJ, YHQ_LXKGVH, YUPDLCOAFY, YZMOUGKVFZ, ZEEZKKHGQC, " +
						"ZFNMCQKHTP, ZIIYLTEMCH, ZIUKNSARCY, ZLZZCTKKBQ, ZMRKZVPOVO, ZRGBZUAFUQ, ZSNFHNDEFV, " +
						"_UFTJTGQWN, _VPRAWSZIJ]"));
	}

	private Set<String> readSymbols_SAX(String fileName) throws Exception {
		final Set<String> result = new TreeSet<String>();

		XMLReader xmlReader = XMLReaderFactory.createXMLReader();
		xmlReader.setContentHandler(new DefaultHandler() {
			@Override
			public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
				if (qName.equals("add")) {
					result.add(attributes.getValue("symbol"));
				}
			}
		});
		xmlReader.parse(new InputSource(new FileReader(fileName)));

		return result;
	}

	private Set<String> readSymbols(String fileName) throws Exception {
		DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder documentBuilder = builderFactory.newDocumentBuilder();
		Document document = documentBuilder.parse(fileName);

		int length = document.getChildNodes().getLength();
		if (length != 1) throw new IllegalStateException();
		Node commands = document.getChildNodes().item(0);
		if (!commands.getNodeName().equals("commands")) throw new IllegalStateException();

		Set<String> symbols = new TreeSet<String>();
		for (int i = 0; i < commands.getChildNodes().getLength(); i++) {
			Node node = commands.getChildNodes().item(i);
			if (!node.getNodeName().equals("add")) continue;

			String symbol = node.getAttributes().getNamedItem("symbol").getNodeValue();
			symbols.add(symbol);
		}
		return symbols;
	}
}
