package at.braun.aoc2021.domain;

import java.util.Arrays;

public class Matrix {
    private String[] names;
    private boolean[][] connections;

    public Matrix() {
        this.names = new String[0];
        this.connections = new boolean[0][0];
    }

    /**
     * Checks, if a node is already in this matrix.
     * 
     * @param name
     * @return
     */
    private boolean containsNode(String name) {
        for (int i = 0; i < names.length; i++) {
            if (name.equals(names[i])) {
                return true;
            }
        }
        return false;
    }

    /**
     * Adds a node to the matrix.
     * 
     * @param name
     */
    private void addNode(String name) {
        if (!containsNode(name)) {
            var length = names.length;
            names = Arrays.copyOf(names, length + 1);
            names[length] = name;
            connections = Arrays.copyOf(connections, length + 1);
            connections[length] = new boolean[length + 1];
            for (int i = 0; i < connections.length; i++) {
                connections[i] = Arrays.copyOf(connections[i], length + 1);
                connections[i][length] = false;
            }
        }
    }

    /**
     * Gets the Index of a node (necessary for access to boolean matrix).
     * 
     * @param node
     * @return
     */
    private int indexOf(String node) {
        for (int i = 0; i < names.length; i++) {
            if (node.equals(names[i])) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Add a connection between 2 nodes.
     * Add these nodes, if they do not exist.
     * 
     * @param name1
     * @param name2
     */
    private void addConnection(String name1, String name2) {
        addNode(name1);
        addNode(name2);
        int index1 = indexOf(name1);
        int index2 = indexOf(name2);
        this.connections[index1][index2] = true;
        this.connections[index2][index1] = true;
    }

    public void addConnectionString(String connectionString) {
        String[] splitConnectionString = connectionString.split("-");
        addConnection(splitConnectionString[0], splitConnectionString[1]);
    }

    @Override
    public String toString() {
        var con = new StringBuilder();
        con.append("\n");
        for (int i = 0; i < connections.length; i++) {
            var arr = connections[i];
            boolean first = true;
            con.append("[");
            for (int j = 0; j < arr.length; j++) {
                if (!first) {
                    con.append(", ");
                }
                first = false;
                con.append(arr[j] ? "1" : "0");
            }
            con.append("]\n");
        }
        return "Matrix { \n names=" + Arrays.toString(names) + "\n, connections=" + con.toString() + "}";
    }

    public boolean[][] getConnections() {
        return this.connections;
    }

    public boolean[] getConnectionsOf(String name) {
        int i = indexOf(name);
        if (i >= 0) {
            return this.connections[i];
        }
        return new boolean[0];
    }

    public int[] getConnectionsIndexOf(String name) {
        boolean[] con = getConnectionsOf(name);
        int[] res = {};
        for (int i = 0; i < con.length; i++) {
            if(con[i]) {
                res = Arrays.copyOf(res, res.length + 1);
                res[res.length - 1] = i;
            }
        }
        return res;
    }

    public String getNameByIndex(int i) {
        return this.names[i];
    }
}
