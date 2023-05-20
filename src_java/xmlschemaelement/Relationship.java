package src_java.xmlschemaelement;

import org.w3c.dom.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;


public class Relationship extends XMLSchemaElement<Relationship> {

    public ArrayList<HashMap<String, String>> references;
    public ArrayList<HashMap<String, String>> allReferences;
    public Relationship inverseRelationship;

    protected Relationship(Node node) {

        super(node);

        this.references = extractReferences(this.itemContentTree.get("references"));

        this.allReferences = new ArrayList<>();
        this.allReferences.addAll(this.references);

        this.taxonomy = extractTaxonomy(itemContentTree.get("taxonomy"));
        this.inverseRelationship = createInverse(node);

    }

    protected Relationship(Node node, Relationship parentRelationship) {

        super(node, parentRelationship);

        this.references = extractReferences(this.itemContentTree.get("references"));

        this.allReferences = new ArrayList<>();
        if (parentRelationship != null) {
            this.allReferences.addAll(parentRelationship.allReferences);
        }
        this.allReferences.addAll(this.references);

        this.taxonomy = extractTaxonomy(itemContentTree.get("taxonomy"));
        this.inverseRelationship = createInverse(node);

    }

    private Relationship(String name, ArrayList<HashMap<String, Object>> attributes,
                         ArrayList<HashMap<String, String>> references,
                         ArrayList<Relationship> taxonomy,
                         Relationship parentRelationship,
                         Relationship inverseRelationship) {

        super(name, attributes, taxonomy, inverseRelationship.itemContentTree, parentRelationship);

        this.references = references;
        this.allReferences = new ArrayList<>();

        if (parentRelationship != null) {
            this.allReferences.addAll(parentRelationship.allReferences);
        }
        this.allReferences.addAll(this.references);

        this.inverseRelationship = inverseRelationship;
    }

    protected HashMap<String, NodeList> gatherContent(Node currentNode) {

        HashMap<String, NodeList> relationshipContent = new HashMap<>();

        NodeList childNodes = currentNode.getChildNodes();

        // simple trick to initialize empty NodeList with a non-existent tag
        NodeList attributeList = ((Element) currentNode).getElementsByTagName("<empty>");
        NodeList referencesList = ((Element) currentNode).getElementsByTagName("<empty>");
        NodeList taxonomyList = ((Element) currentNode).getElementsByTagName("<empty>");

        for (int i = 0; i < currentNode.getChildNodes().getLength(); i++) {

            Node child = childNodes.item(i);
            if (child instanceof Element childElement) {

                switch (childElement.getTagName()) {
                    case "attributes" -> attributeList = child.getChildNodes();
                    case "references" -> referencesList = child.getChildNodes();
                    case "taxonomy" -> taxonomyList = child.getChildNodes();
                }
            }

        }

        relationshipContent.put("attributes", attributeList);
        relationshipContent.put("references", referencesList);
        relationshipContent.put("taxonomy", taxonomyList);

        return relationshipContent;
    }

    private Relationship createInverse(Node node) {

        String inverseName = node.getAttributes().getNamedItem("inverse").getNodeValue();

        if (inverseName.equals(this.name)) {
            return null;
        }

        ArrayList<HashMap<String, String>> invertedReferences = new ArrayList<>();

        for (HashMap<String, String> reference : this.references) {
            HashMap<String, String> invertedReference = new HashMap<>();
            invertedReference.put("subject", reference.get("object"));
            invertedReference.put("object", reference.get("subject"));

            invertedReferences.add(invertedReference);
        }

        ArrayList<Relationship> invertedTaxonomy = new ArrayList<>();

        for (Relationship taxonomyRel: this.taxonomy) {
            invertedTaxonomy.add(Objects.requireNonNullElse(taxonomyRel.inverseRelationship, taxonomyRel));
        }

        Relationship invertedParent = this.parent;
        if (this.parent != null && this.parent.inverseRelationship != null) {
            invertedParent = this.parent.inverseRelationship;
        }

        return new Relationship(
                inverseName,
                this.attributes,
                invertedReferences,
                invertedTaxonomy,
                invertedParent,
                this
        );
    }

    private ArrayList<HashMap<String, String>> extractReferences(NodeList referencesNodeList) {

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

    protected ArrayList<Relationship> extractTaxonomy(NodeList taxonomiesNodeList) {

        ArrayList<Relationship> taxonomy = new ArrayList<>();

        for (int i = 0; i < taxonomiesNodeList.getLength(); i++) {
            Node node = taxonomiesNodeList.item(i);

            if (node.getNodeType() != Node.TEXT_NODE) {

                Relationship subClassRel = new Relationship(node, this);
                taxonomy.add(subClassRel);
            }
        }

        return taxonomy;
    }

}
