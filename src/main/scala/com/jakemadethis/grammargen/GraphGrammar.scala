package com.jakemadethis.grammargen;

trait Hypergraph[V, E] {
  def addEdge(edge: E, vertices: Seq[V]): Hypergraph[V, E]
  def addVertex(vertex: V): Hypergraph[V, E]

  def removeVertex(vertex: V): Hypergraph[V, E]
  def removeEdge(edge: E): Hypergraph[V, E]

  def edges: Set[E]
  def vertices: Set[V]
  def neighbors(vertex: V): Set[V]
  def incidentEdges(vertex: V): Set[E]
  def incidentVertices(edge: E): Seq[V]

  def containsVertex(vertex: V) = vertices contains vertex
  def containsEdge(edge: E) = edges contains edge
  def edgeCount = edges.size
  def vertexCount = vertices.size
  def isIncident(vertex: V, edge: E) = incidentEdges(vertex) contains edge

  final def +(vertex: V) = addVertex(vertex)
  final def -(vertex: V) = removeVertex(vertex)
}

object Hypergraph {
  def apply[V, E]() = new OrderedHypergraph[V, E](Set(), Set(), Map(), Map())
  def apply[V, E](v: V, vn: V*) = new OrderedHypergraph[V, E]((vn :+ v).toSet, Set(), Map(), Map())
}

class OrderedHypergraph[V, E](
  val vertices: Set[V], 
  val edges: Set[E],
  edgeMap: Map[E, Seq[V]],
  vertexMap: Map[V, Set[E]]) extends Hypergraph[V, E] {

  def incidentEdges(vertex: V) = vertexMap(vertex)
  def incidentVertices(edge: E) = edgeMap(edge)

  def neighbors(vertex: V) = ???

  def addEdge(edge: E, attached: Seq[V]) = {
    if (containsEdge(edge)) {
      if (edgeMap(edge) == attached) this
      else throw new IllegalArgumentException("Edge already exists with different endpoints")
    } else
      new OrderedHypergraph[V, E](vertices, edges + edge,
        edgeMap + (edge -> attached),
        mapVertexIncidents(attached) { (v, edges) => edges + edge })
  }

  def addVertex(vertex: V) = 
    new OrderedHypergraph[V, E](vertices + vertex, edges, edgeMap, vertexMap)

  def removeEdge(edge: E) = {
    new OrderedHypergraph[V,E](vertices, edges - edge, 
      edgeMap - edge,
      mapVertexIncidents(edgeMap(edge)) { (v, edges) => edges - edge })
  }

  def removeVertex(vertex: V) = {
    // todo remove edges
    new OrderedHypergraph[V, E](
      vertices - vertex, edges,
      edgeMap, vertexMap - vertex)
  }

  private def mapVertexIncidents(vertices: Seq[V])(f: (V, Set[E]) => Set[E]) = {
    (vertices :\ vertexMap) { (v, resultVertexMap) => 
      resultVertexMap + (v -> f(v, resultVertexMap get v getOrElse Set()))
    }
  }
  private def mapEdgeIncidents(edges: Seq[E])(f: (E, Seq[V]) => Seq[V]) = {
    (edges :\ edgeMap) { (e, resultEdgeMap) => 
      resultEdgeMap + (e -> f(e, resultEdgeMap get e getOrElse Seq()))
    }
  }

}