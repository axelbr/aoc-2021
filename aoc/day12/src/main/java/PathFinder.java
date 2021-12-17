import java.util.*;
import java.util.function.BiPredicate;
import java.util.stream.Collectors;

public class PathFinder {

    public static final String START = "start";
    public static final String END = "end";
    private final BiPredicate<HashMap<String, Integer>, String> visitingPolicy;

    public PathFinder(BiPredicate<HashMap<String, Integer>, String> visitingPolicy) {
        this.visitingPolicy = visitingPolicy;
    }

    private record Node(String cave, Node parent, Map<String, Integer> visited) {
    }
    
    private List<List<String>> backtrack(List<Node> leafs) {
        List<List<String>> paths = new ArrayList<>();
        for (Node leaf : leafs) {
            Node current = leaf;
            List<String> path = new LinkedList<>(List.of(current.cave));
            while (current.parent != null) {
                current = current.parent;
                path.add(0, current.cave);
            }
            paths.add(path);
        }
        return paths;
    }


    public List<List<String>> findAllPaths(Map<String, Set<String>> caves) {
        Node searchTree = new Node(START, null, new HashMap<>());
        Queue<Node> queue = caves.get(START).stream().map(s -> new Node(s, searchTree, Map.of(START, 1))).collect(Collectors.toCollection(LinkedList::new));
        List<Node> endNodes = new ArrayList<>();
        while (!queue.isEmpty()) {
            var node = queue.poll();
            if (node.cave.equals(END)) {
                endNodes.add(node);
            } else {
                for (String neighbour : caves.get(node.cave)) {
                    var visited = new HashMap<>(node.visited);
                    visited.put(node.cave, visited.getOrDefault(node.cave, 0) + 1);
                    if (this.visitingPolicy.test(visited, neighbour)) {
                        queue.add(new Node(neighbour, node, visited));
                    }
                }
            }
        }
        return backtrack(endNodes);
    }
}
