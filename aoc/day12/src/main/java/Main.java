import javax.xml.namespace.QName;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.function.Predicate;

public class Main {

    public static Map<String, Set<String>> parseCaves(String path) throws IOException {
        Predicate<String> isBig = name -> name.toUpperCase().equals(name);
        try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
            Map<String, Set<String>> caves = new HashMap<>();
            for (var line : reader.lines().toList()) {
                String from = line.split("-")[0];
                String to = line.split("-")[1];
                if (!caves.containsKey(from)) {
                    caves.put(from, new HashSet<>());
                }
                if (!caves.containsKey(to)) {
                    caves.put(to, new HashSet<>());
                }
                caves.get(from).add(to);
                caves.get(to).add(from);
            }
            return caves;
        }
    }

    private static boolean isBig(String name) {
        return name.toUpperCase().equals(name);
    }

    static boolean part1VisitingPolicy(HashMap<String, Integer> visited, String neighbour) {
        return isBig(neighbour) || !visited.containsKey(neighbour);
    }

    static boolean part2VisitingPolicy(HashMap<String, Integer> visited, String neighbour) {
        var multiVisitsSmallCave = visited.entrySet().stream().filter(e -> !isBig(e.getKey()) && e.getValue() > 1).count();
        return isBig(neighbour) || !visited.containsKey(neighbour) || (multiVisitsSmallCave == 0 && !neighbour.equals("start"));
    }

    public static void main(String[] args) throws IOException {
        Map<String, Set<String>> caves = parseCaves(args[0]);
        var paths = new PathFinder(Main::part1VisitingPolicy).findAllPaths(caves);
        //paths.forEach(System.out::println);
        System.out.println("Part 1 | Number of paths: " + paths.size());
        paths = new PathFinder(Main::part2VisitingPolicy).findAllPaths(caves);
        //paths.forEach(System.out::println);
        System.out.println("Part 2 | Number of paths: " + paths.size());

    }
}
