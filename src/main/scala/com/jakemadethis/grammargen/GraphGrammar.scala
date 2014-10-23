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


  def foldEdges[A, B](edges: Seq[A])(f: (A, Seq[B]) => Seq[B]) = {
    (edges :\ Map[A, Seq[B]]()) { (e, res) => 
      res + (e -> f(e, res get e getOrElse Seq()))
    }
  }
  def foldVertices[A, B](vertices: Seq[A])(f: Set[B] => Set[B]) = {
    (vertices :\ Map[A, Set[B]]()) { (v, res) => 
      res + (v -> f(res get v getOrElse Set()))
    }
  }
}

class Handle[V, E](val edge: E, vertices: Seq[V]) 
  extends OrderedHypergraph[V, E](
    vertices  = vertices.toSet, 
    edges     = Set(edge),
    edgeMap   = Map(edge -> vertices),
    vertexMap = Hypergraph.foldVertices(vertices)(_ + edge)) {
  val size = vertices.size
}

class OrderedHypergraph[V, E](
  val vertices: Set[V], 
  val edges: Set[E],
  edgeMap: Map[E, Seq[V]],
  vertexMap: Map[V, Set[E]]) extends Hypergraph[V, E] {

  import Hypergraph._

  def incidentEdges(vertex: V) = vertexMap(vertex )
  def incidentVertices(edge: E) = edgeMap(edge)

  def neighbors(vertex: V) = ???

  def addEdge(edge: E, attached: Seq[V]) = {
    if (!containsEdge(edge)) 
      new OrderedHypergraph[V, E](vertices, edges + edge,
        edgeMap + (edge -> attached),
        vertexMap ++ foldVertices[V, E](attached)(_ + edge))
    else if (edgeMap(edge) != attached) 
      throw new IllegalArgumentException("Edge already exists with different endpoints")
    else this
  }

  def addVertex(vertex: V) = 
    new OrderedHypergraph[V, E](vertices + vertex, edges, edgeMap, vertexMap)

  def removeEdge(edge: E) = {
    new OrderedHypergraph[V,E](vertices, edges - edge, 
      edgeMap - edge,
      vertexMap ++ foldVertices[V, E](edgeMap(edge))(_ - edge))
  }

  def removeVertex(vertex: V) = {
    // todo remove edges
    new OrderedHypergraph[V, E](
      vertices - vertex, edges,
      edgeMap, vertexMap - vertex)
  }

}