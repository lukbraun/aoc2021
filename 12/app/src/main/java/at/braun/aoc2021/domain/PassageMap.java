package at.braun.aoc2021.domain;

import java.util.Arrays;

import at.braun.aoc2021.domain.strategy.VisitFilter;

public class PassageMap {
    private final Matrix connections;
    private String[][] results;

    public PassageMap(String[] connectionString) {
        this.connections = new Matrix();
        for (int i = 0; i < connectionString.length; i++) {
            this.connections.addConnectionString(connectionString[i]);
        }
        results = new String[0][0];
    }

    private boolean pathAlreadyChecked(String[] path) {
        for (int i = 0; i < results.length; i++) {
            String[] pathToCheck = results[i];
            if (path.length == pathToCheck.length) {
                boolean isSame = true;
                for (int j = 0; j < pathToCheck.length; j++) {
                    if (!pathToCheck[j].equals(path[j])) {
                        isSame = false;
                        break;
                    }
                }
                if (isSame) {
                    return true;
                }
            }
        }
        return false;
    }

    private static void printPath(String path[]) {
        boolean first = true;
        var sb = new StringBuilder();
        for (int i = 0; i < path.length; i++) {
            if(!first) {
                sb.append(",");
            }
            first = false;
            sb.append(path[i]);
        }
        sb.append(",end");
        System.out.println(sb);
    }

    private int getNumberOfConnections(final VisitFilter visitFilter, String start, String end, String current,
            String[] visited, int res) {
        if (end.equals(current)) {
            // printPath(visited);
            if (!pathAlreadyChecked(visited)) {
                return res + 1;
            }
            return res;
        }
        int[] connectionIndices = this.connections.getConnectionsIndexOf(current);
        visited = Arrays.copyOf(visited, visited.length + 1);
        visited[visited.length - 1] = current;
        for (int i = 0; i < connectionIndices.length; i++) {
            String toVisit = this.connections.getNameByIndex(connectionIndices[i]);
            if (visitFilter.filter(visited, toVisit)) {
                res = getNumberOfConnections(visitFilter, start, end, toVisit, visited, res);
            }
        }
        return res;
    }

    public int getNumberOfConnections(final VisitFilter visitFilter, String start, String end) {
        return getNumberOfConnections(visitFilter, start, end, start, new String[0], 0);
    }
}
