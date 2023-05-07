package src_java.xmlschemaelement;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.HashMap;


abstract class XMLSchemaElement {

    public String name;
    public ArrayList<HashMap<String, String>> attributes;
    protected HashMap<String, NodeList> itemContentTree;

    public XMLSchemaElement(Node itemNode) {

        this.name = gatherName(itemNode);
        this.itemContentTree = gatherContent(itemNode);

        this.attributes = extractAttributes(itemContentTree.get("attributes"));

    }

    public XMLSchemaElement(String name, ArrayList<HashMap<String, String>> attributes) {

        this.name = name;
        this.attributes = attributes;

    }

    public String gatherName(Node itemNode) {

        return itemNode.getAttributes().getNamedItem("name").getNodeValue();
    }

    public abstract HashMap<String, NodeList> gatherContent(Node itemNode);

    public ArrayList<HashMap<String, String>> extractAttributes(NodeList attributesNodeList) {

        ArrayList<HashMap<String, String>> attributes = new ArrayList<>();

        for (int i = 0; i < attributesNodeList.getLength(); i++) {
            Node node = attributesNodeList.item(i);
            if (node.getNodeType() != Node.TEXT_NODE) {

                NamedNodeMap nodeAttributes = node.getAttributes();

                HashMap<String, String> attribute = new HashMap<>();

                for (int j = 0; j < nodeAttributes.getLength(); j++) {

                    String key = nodeAttributes.item(j).getNodeName();
                    String value = nodeAttributes.item(j).getNodeValue();

                    attribute.put(key, value);
                }

                attributes.add(attribute);
            }
        }

        return attributes;
    }

}
