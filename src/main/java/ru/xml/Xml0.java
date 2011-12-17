package ru.xml;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.util.Set;
import java.util.TreeSet;

/**
 * User: dima
 * Date: 16/4/11
 */
public class Xml0 {
	@Test public void getAllSymbolsUsingDOM() throws ParserConfigurationException, IOException, SAXException {
		Set<String> symbolSet = new TreeSet<String>();

		DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder documentBuilder = builderFactory.newDocumentBuilder();
		Document document = documentBuilder.parse("orders2.xml");

		Node commandsNode = document.getChildNodes().item(0);
		if (!commandsNode.getNodeName().equals("commands")) throw new IllegalStateException();

		NodeList actionNodes = commandsNode.getChildNodes();
		for (int i = 0; i < actionNodes.getLength(); i++) {
			Node node = actionNodes.item(i);
			if (node.getNodeType() != Node.ELEMENT_NODE) continue;

			if (!node.getNodeName().equals("add") && !node.getNodeName().equals("edit") && // used || instead of &&
					!node.getNodeName().equals("remove")) {
				throw new IllegalStateException("Unexpected node name for node: " + node);
			}
			if (!node.getNodeName().equals("add")) continue;

			Node symbolAttribute = node.getAttributes().getNamedItem("symbol"); // got NPE because checked for null value, not Node
			if (symbolAttribute == null) throw new IllegalStateException(); // forgot to check for "remove" action (later code was changed)
			symbolSet.add(symbolAttribute.getNodeValue());
		}

		Assert.assertEquals(symbolSet.toString(),
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
						"_UFTJTGQWN, _VPRAWSZIJ]");
	}
}
