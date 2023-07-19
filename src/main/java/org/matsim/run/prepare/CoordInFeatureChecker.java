package org.matsim.run.prepare;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.network.Link;
import org.matsim.core.utils.gis.ShapeFileReader;
import org.opengis.feature.simple.SimpleFeature;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/*
@author dziemke
 */
public class CoordInFeatureChecker {
    private String shapeFile;
    private Map<String, Geometry> featureMap = new HashMap<>();

    GeometryFactory geometryFactory = new GeometryFactory();

    public static void main(String[] args) {
        String shapeFile = "../shapefile.shp";

        CoordInFeatureChecker coordInFeatureChecker = new CoordInFeatureChecker(shapeFile);

        Link link = null;
        boolean answer = coordInFeatureChecker.checkIfLinkInFeature(link, "12345");
    }

    public CoordInFeatureChecker(String shapeFile) {
        this.shapeFile = shapeFile;
        Collection<SimpleFeature>  allFeatures = ShapeFileReader.getAllFeatures(shapeFile);

        for (SimpleFeature feature : allFeatures) {
            Geometry geometry = (Geometry) feature.getDefaultGeometry();
            featureMap.put((String) feature.getAttribute("Spaltenkopf_der_ID-Spalte"), geometry);
        }
    }

    public boolean checkIfLinkInFeature(Link link, String featureId) {
        Geometry geometry = featureMap.get(featureId);

        Coord linkCenter = link.getCoord();
        Coordinate coordinate = new Coordinate(linkCenter.getX(), linkCenter.getY());
        Point point = geometryFactory.createPoint(coordinate);

        boolean isFeatureContainsPoint = false;
        if (geometry.contains(point)) {
            isFeatureContainsPoint = true;
        }
        return isFeatureContainsPoint;
    }
}
