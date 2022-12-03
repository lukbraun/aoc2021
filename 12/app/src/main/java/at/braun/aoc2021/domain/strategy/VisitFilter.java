package at.braun.aoc2021.domain.strategy;

public interface VisitFilter {
    boolean filter(String[] visited, String node);
}
