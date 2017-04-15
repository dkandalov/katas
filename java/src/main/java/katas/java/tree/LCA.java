package katas.java.tree;

import katas.java.tree.common.Node;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static katas.java.tree.common.Node.node;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.Assert.assertThat;

/**
 * See https://www.hackerrank.com/challenges/binary-search-tree-lowest-common-ancestor
 */
public class LCA {
    @Test public void findPathToData() {
        Node<Integer> tree =
            node(4,
                node(2, node(1), node(3)),
                node(7, node(6), null)
            );
        List<Integer> path = pathTo(3, tree).stream().map(it -> (Integer) it.data).collect(Collectors.toList());
        assertThat(path, equalTo(Arrays.asList(4, 2, 3)));
    }

    @Test public void findLowestCommonAncestor() {
        Node<Integer> tree =
            node(4,
                node(2, node(1), node(3)),
                node(7, node(6), null)
            );

        assertThat(lca(tree, 4, 4).data, equalTo(4));
        assertThat(lca(tree, 4, 2).data, equalTo(4));
        assertThat(lca(tree, 2, 4).data, equalTo(4));
        assertThat(lca(tree, 2, 7).data, equalTo(4));
        assertThat(lca(tree, 1, 7).data, equalTo(4));

        assertThat(lca(tree, 2, 2).data, equalTo(2));
        assertThat(lca(tree, 1, 3).data, equalTo(2));
    }

    @SuppressWarnings("WeakerAccess")
    static Node lca(Node root, int v1, int v2) {
        List<Node> path1 = pathTo(v1, root);
        List<Node> path2 = pathTo(v2, root);

        Node result = path1.get(0);
        while (path1.size() > 0 && path2.size() > 0 && path1.get(0).equals(path2.get(0))) {
            result = path1.get(0);
            path1.remove(0);
            path2.remove(0);
        }
        return result;
    }

    private static List<Node> pathTo(int data, Node node) {
        if (node == null) return null;
        if (node.data.equals(data)) {
            ArrayList<Node> list = new ArrayList<>();
            list.add(node);
            return list;
        }

        List<Node> path = pathTo(data, node.left);
        if (path == null) path = pathTo(data, node.right);

        if (path != null) {
            path.add(0, node);
            return path;
        } else {
            return null;
        }
    }
}
