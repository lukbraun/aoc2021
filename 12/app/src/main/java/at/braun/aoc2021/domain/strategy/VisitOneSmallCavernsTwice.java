package at.braun.aoc2021.domain.strategy;

public class VisitOneSmallCavernsTwice implements VisitFilter {

    private boolean alreadyVisitedOneCavernTwice(String[] visited) {
        for (int i = 0; i < visited.length; i++) {
            String current = visited[i];
            if (Character.isLowerCase(current.charAt(0))) {
                for (int j = 0; j < visited.length; j++) {
                    if (i != j && current.equals(visited[j])) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private boolean visitedLessThanTwice(String[] visited, String node) {
        int noVisitsSmall = 0;
        if (Character.isLowerCase(node.charAt(0))) {
            for (int i = 0; i < visited.length; i++) {
                if (node.equals(visited[i])) {
                    noVisitsSmall++;
                }
            }
        }
        return noVisitsSmall < 2;
    }

    @Override
    public boolean filter(String[] visited, String node) {
        if (node.equals("start")) {
            return false;
        }
        if (alreadyVisitedOneCavernTwice(visited)) {
            return new VisitBigCavernsOnce().filter(visited, node);
        }
        return visitedLessThanTwice(visited, node);
    }

}
