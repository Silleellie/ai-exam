package src_java.xmlschemaelement;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.HashMap;


class Relationship extends XMLSchemaElement {

    public ArrayList<HashMap<String, String>> references;
    public Relationship inverseRelationship;

    public Relationship(Node node) {

        super(node);

        this.references = extractReferences(itemContentTree.get("references"));
        this.inverseRelationship = createInverse(node);

    }

    public Relationship(String name, ArrayList<HashMap<String, String>> attributes,
                        ArrayList<HashMap<String, String>> references,
                        Relationship relationship) {

        super(name, attributes);
        this.references = references;
        this.inverseRelationship = relationship;
    }


    public Relationship createInverse(Node node) {

        String inverseName = node.getAttributes().getNamedItem("inverse").getNodeValue();

        if (inverseName.equals(this.name)) {
            return null;
        }

        ArrayList<HashMap<String, String>> invertedReferences = new ArrayList<>();

        for (HashMap<String, String> reference : references) {
            HashMap<String, String> invertedReference = new HashMap<>();
            invertedReference.put("subject", reference.get("object"));
            invertedReference.put("object", reference.get("subject"));

            invertedReferences.add(invertedReference);
        }

        return new Relationship(inverseName, attributes, invertedReferences, this);
    }

    public HashMap<String, NodeList> gatherContent(Node currentNode) {

        HashMap<String, NodeList> entityContent = new HashMap<>();

        NodeList childNodes = currentNode.getChildNodes();

        // simple trick to initialize empty NodeList with a non-existent tag
        NodeList attributeList = ((Element) currentNode).getElementsByTagName("<empty>");
        NodeList referencesList = ((Element) currentNode).getElementsByTagName("<empty>");

        for (int i = 0; i < currentNode.getChildNodes().getLength(); i++) {

            Node child = childNodes.item(i);
            if (child instanceof Element childElement) {

                if (childElement.getTagName().equals("attributes")) {

                    attributeList = child.getChildNodes();

                } else if (childElement.getTagName().equals("references")) {

                    referencesList = child.getChildNodes();
                }
            }

        }

        entityContent.put("attributes", attributeList);
        entityContent.put("references", referencesList);

        return entityContent;
    }

    public ArrayList<HashMap<String, String>> extractReferences(NodeList referencesNodeList) {

        ArrayList<HashMap<String, String>> references = new ArrayList<>();

        for (int i = 0; i < referencesNodeList.getLength(); i++) {
            Node node = referencesNodeList.item(i);
            if (node.getNodeType() != Node.TEXT_NODE) {

                NamedNodeMap nodeReferences = node.getAttributes();

                HashMap<String, String> reference = new HashMap<>();

                String subj = nodeReferences.getNamedItem("subject").getNodeValue();
                String obj = nodeReferences.getNamedItem("object").getNodeValue();

                reference.put("subject", subj);
                reference.put("object", obj);
                references.add(reference);
            }
        }

        return references;
    }

}
