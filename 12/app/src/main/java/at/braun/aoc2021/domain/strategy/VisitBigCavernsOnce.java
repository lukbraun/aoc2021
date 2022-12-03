package at.braun.aoc2021.domain.strategy;

public class VisitBigCavernsOnce implements VisitFilter {

    @Override
    public boolean filter(String[] visited, String node) {
        if (Character.isLowerCase(node.charAt(0))) {
            for (int i = 0; i < visited.length; i++) {
                if (node.equals(visited[i])) {
                    return false;
                }
            }
        }
        return true;
    }

}
