package at.braun.aoc2021.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import at.braun.aoc2021.domain.strategy.VisitBigCavernsOnce;
import at.braun.aoc2021.domain.strategy.VisitOneSmallCavernsTwice;

public class PassageMapTest {

    @Test
    public void testEasy1() {
        String[] input = { "start-A",
                "start-b",
                "A-c",
                "A-b",
                "b-d",
                "A-end",
                "b-end" };
        var passageMap = new PassageMap(input);
        assertEquals(10, passageMap.getNumberOfConnections(new VisitBigCavernsOnce(), "start", "end"));
    }

    @Test
    public void testLarger1() {
        String[] input = { "fs-end",
                "he-DX",
                "fs-he",
                "start-DX",
                "pj-DX",
                "end-zg",
                "zg-sl",
                "zg-pj",
                "pj-he",
                "RW-he",
                "fs-DX",
                "pj-RW",
                "zg-RW",
                "start-pj",
                "he-WI",
                "zg-he",
                "pj-fs",
                "start-RW" };
        var passageMap = new PassageMap(input);
        assertEquals(226, passageMap.getNumberOfConnections(new VisitBigCavernsOnce(), "start", "end"));
    }

    @Test
    public void testActual1() {
        String[] input = { "zi-end",
                "XR-start",
                "zk-zi",
                "TS-zk",
                "zw-vl",
                "zk-zw",
                "end-po",
                "ws-zw",
                "TS-ws",
                "po-TS",
                "po-YH",
                "po-xk",
                "zi-ws",
                "zk-end",
                "zi-XR",
                "XR-zk",
                "vl-TS",
                "start-zw",
                "vl-start",
                "XR-zw",
                "XR-vl",
                "XR-ws"
        };
        var passageMap = new PassageMap(input);
        assertEquals(3761, passageMap.getNumberOfConnections(new VisitBigCavernsOnce(), "start", "end"));
    }

    @Test
    public void testEasy2() {
        String[] input = { "start-A",
                "start-b",
                "A-c",
                "A-b",
                "b-d",
                "A-end",
                "b-end" };
        var passageMap = new PassageMap(input);
        assertEquals(36, passageMap.getNumberOfConnections(new VisitOneSmallCavernsTwice(), "start", "end"));
    }

    @Test
    public void testLarger2() {
        String[] input = { "fs-end",
                "he-DX",
                "fs-he",
                "start-DX",
                "pj-DX",
                "end-zg",
                "zg-sl",
                "zg-pj",
                "pj-he",
                "RW-he",
                "fs-DX",
                "pj-RW",
                "zg-RW",
                "start-pj",
                "he-WI",
                "zg-he",
                "pj-fs",
                "start-RW" };
        var passageMap = new PassageMap(input);
        assertEquals(3509, passageMap.getNumberOfConnections(new VisitOneSmallCavernsTwice(), "start", "end"));
    }

    @Test
    public void testActual2() {
        String[] input = { "zi-end",
                "XR-start",
                "zk-zi",
                "TS-zk",
                "zw-vl",
                "zk-zw",
                "end-po",
                "ws-zw",
                "TS-ws",
                "po-TS",
                "po-YH",
                "po-xk",
                "zi-ws",
                "zk-end",
                "zi-XR",
                "XR-zk",
                "vl-TS",
                "start-zw",
                "vl-start",
                "XR-zw",
                "XR-vl",
                "XR-ws"
        };
        var passageMap = new PassageMap(input);
        assertEquals(99138, passageMap.getNumberOfConnections(new VisitOneSmallCavernsTwice(),
                "start", "end"));
    }
}
