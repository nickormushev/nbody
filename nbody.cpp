#include <iostream>
#include <numeric>
#include <vector>
#include <mpi.h>
#include <cstddef>
#include <tgmath.h>
#include <algorithm>
#include <chrono>

#define spaceX 10000        		          //the maximum x cordinate of space
#define spaceY 10000        		          //the maximum y coordinate of space
#define bodyCount 1600000    		          //how many bodies to simulate
#define MAX_RADIUS 3        		          //the maximum initial radius
#define MAX_VELOCITY 10     		          //the maximum allowed initial velocity
#define MASTER 0            		          //the processor with id 0
#define TIME 10             		          //how many units of time should we simulate
#define DELTAT 0.1          		          //one unit of time
#define ACCURACY 0.2        		          //how accurate are the barnes hut approximations
#define GRAVITY 6.67300e-11 		          //the gravity constand
#define MAX_MASS 1000       		          //the maximum mass of the bodies
#define BODIES_PER_LEAF 3 * bodyCount/100     //the amount of bodies stored in a barnes hut tree leaf
#define ORB_SPLIT_ERROR 0.1                   //the allowed difference between the workload and 0.5 during ORB

MPI_Datatype TMPIMessageBody;
//Contains only the essential data for the calculations that is sent between processors
struct MessageBody {
    double x, y, mass;

    MessageBody() {
        this->x = 0;
        this->y = 0;
        this->mass = 0;
    }

    MessageBody(double x, double y, double mass) {
        this->x = x;
        this->y = y;
        this->mass = mass;
    }

    void simplePrint() {
        std::cout << "x, y, mass: (" << x << "," << y << "," << mass << ")" << std::endl;
    }
};

MPI_Datatype TMPIBody;
struct Body {
    int id, weight;
    double x, y;
    double velocityX, velocityY, radius, mass;
    double forceX, forceY;

    void copy(int id, int weight, double x, double y, double velocityX, double velocityY,
            double radius, double mass, double forceX, double forceY) {
       this->x = x;
       this->y = y;
       this->mass = mass;
       this->id = id;
       this->weight = weight;
       this->velocityX = velocityX;
       this->velocityY = velocityY;
       this->forceX = forceX;
       this->forceY = forceY;
    }

    Body() {
        copy(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    }
    
    Body(MessageBody o) {
        copy(0, 0, o.x, o.y, 0, 0, 0, o.mass, 0, 0);
    }
};

void logTime(std::chrono::high_resolution_clock::time_point start, std::string logMessage) {
    MPI_Barrier(MPI_COMM_WORLD);
    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
    std::cout << duration.count() << " " << logMessage << std::endl;
    MPI_Barrier(MPI_COMM_WORLD);
}

bool in_borders(Body body, double minX, double minY, double maxX, double maxY) {
    return body.x >= minX && body.x <= maxX && body.y >= minY && body.y <= maxY;
}

bool is_below(Body body, double split, double minX, double minY, double maxX, double maxY, bool isXSplit) {
    return (isXSplit && in_borders(body, minX, minY, split, maxY)) 
                || (!isXSplit && in_borders(body, minX, minY, maxX, split));
}

bool is_above(Body body, double split, double minX, double minY, double maxX, double maxY, bool isXSplit) {
    return (isXSplit && in_borders(body, split, minY, maxX, maxY))
        || (!isXSplit && in_borders(body, minX, split, maxX, maxY));
}

bool processorLeftOfSplit(double x, double y, double splitCoordinate, bool isXSplit) {
    return ((isXSplit && x <= splitCoordinate) 
            || (!isXSplit && y <= splitCoordinate));
}

void printBodyVectorIds(int process_Rank, std::vector<Body> v) {
    for (int i = 0; i < v.size(); ++i) {
        std::cout << v[i].id << " " << v[i].x << " " << v[i].y << " rank:" << process_Rank << std::endl;
    }
}

template<typename T>
void printVectorGeneric(std::vector<T> v) {
    for (int i = 0; i < v.size(); ++i) {
        v[i].simplePrint();
    }

    std::cout << std::endl;
}

void printVector(std::vector<int> v) {
    for (int i = 0; i < v.size(); ++i) {
        std::cout << v[i] << " ";
    }
    std::cout << std::endl;
}

std::vector<int> getBinary(int number) {
    std::vector<int> binary;
    int size_Of_Cluster;
    MPI_Comm_size(MPI_COMM_WORLD, &size_Of_Cluster);

    if(number != 0) {
        while(number > 0) {
            binary.push_back(number % 2);
            number /= 2;
        }
    }

    int size = log2(size_Of_Cluster);
    while(binary.size() < size) {
        binary.push_back(0);
    }

    return binary;
}

int fromBinary(std::vector<int> binary) {
    int decimal = 0;
    for (int i = 0; i < binary.size(); ++i) {
        decimal += binary[i] * pow(2, i);
    }

    return decimal;
}

//swaps zero and one at the bitPosition
//of the bit that was used to make the split
//By doing this we receive a neighbor with
//which we can communicate.
int getNeighborFromBit(int bitPosition) {
    int process_Rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &process_Rank);

    std::vector<int> pRankInBinary = getBinary(process_Rank);
    if(pRankInBinary[bitPosition] == 1) {
        pRankInBinary[bitPosition] = 0;
    } else {
        pRankInBinary[bitPosition] = 1;
    }

    return fromBinary(pRankInBinary);
}

//returns the processor global number
int getProcessorWorldRank(int numProcessor, MPI_Group processorGroup) {
    int currentRank[1] = { numProcessor };
    int worldRank[1];
    MPI_Group worldGroup;
    MPI_Comm_group(MPI_COMM_WORLD, &worldGroup);
    //translate local group id's to global group id's also called world ranks
    MPI_Group_translate_ranks(processorGroup, 1, currentRank, worldGroup, worldRank);

    return worldRank[0];
}

struct BHCell {
    bool isLeaf;
    double x, y;
    double cmass_x, cmass_y;
    double mass, width, height;
    std::vector<Body> bodies;

    BHCell* subcells[4];

    void destroy() {
        for (int i = 0; i < 4; ++i) {
            if(subcells[i] != nullptr) {
                delete subcells[i];
            }
        }
    }

    void copy(double x, double y, double width, double height, double isLeaf, double mass,
            double cmass_x, double cmass_y, std::vector<Body> bodies, BHCell * const subcells[4]) {
        this->x = x;       
        this->y = y;
        this->width = width;
        this->height = height;
        this->isLeaf = true;
        this->mass = mass;
        this->cmass_x = cmass_x;
        this->cmass_y = cmass_y;

        this->bodies = bodies;

        //for (int i = 0; i < bodies.size(); ++i) {
        //    if(in_borders(bodies[i], x, y, x + width, y + height)) {
        //        this->bodies.push_back(bodies[i]);
        //    }
        //}

        for (int i = 0; i < 4; ++i) {
             this->subcells[i] = new BHCell(*subcells[i]);
        }
    }

    BHCell() {
        this->x = 0;
        this->y = 0;
        this->width = spaceX;
        this->height = spaceY;
        this->isLeaf = true;

        this->mass = 0;
        this->cmass_x = 0;
        this->cmass_y = 0;

        for (int i = 0; i < 4; ++i) {
            subcells[i] = nullptr;
        }
    }

    BHCell(double x, double y, double width, double height, std::vector<Body> bodies) {
        this->x = x;       
        this->y = y;
        this->width = width;
        this->height = height;
        this->isLeaf = true;
        this->mass = 0;
        this->cmass_x = 0;
        this->cmass_y = 0;

        for (int i = 0; i < bodies.size(); ++i) {
            if(in_borders(bodies[i], x, y, x + width, y + height)) {
                this->bodies.push_back(bodies[i]);
            }
        }

        for (int i = 0; i < 4; ++i) {
            this->subcells[i] = nullptr;
        }
    }

    BHCell(const BHCell& other) {
        this->copy(other.x, other.y, other.width, other.height, other.isLeaf,
                other.mass, other.cmass_x, other.cmass_x, other.bodies, other.subcells);
    }

    BHCell& operator=(const BHCell &other) {
        if (this != &other) {
            this->destroy();
            this->copy(other.x, other.y, other.width, other.height, other.isLeaf,
                    other.mass, other.cmass_x, other.cmass_x, other.bodies, other.subcells);
        }
    
        return *this;
    }

    ~BHCell() {
        this->destroy();
    }

    void calculateMetrics() {
	    if(this->bodies.size() <= BODIES_PER_LEAF) {
	    	return;
	    }

    	int process_Rank;
    	MPI_Comm_rank(MPI_COMM_WORLD, &process_Rank);
 
        this->mass = 0;
        double bodyByMassSumX = 0;
        double bodyByMassSumY = 0;

        for (int i = 0; i < this->bodies.size(); ++i) {
            this->mass += this->bodies[i].mass;
            bodyByMassSumX += this->bodies[i].mass * this->bodies[i].x;
            bodyByMassSumY += this->bodies[i].mass * this->bodies[i].y;
        }

        this->cmass_x = bodyByMassSumX/this->mass;
        this->cmass_y = bodyByMassSumY/this->mass;

        for (int i = 0; i < 4; ++i) {
            subcells[i]->calculateMetrics();
        }
	
    }

    bool bodyInBHCell(Body b) {
        return in_borders(b, this->x, this->y, this->x + this->width, this->y + this->height);
    }

    BHCell* getChildForBody(Body b) {
        for (int i = 0; i < 4; ++i) {
            if(this->subcells[i]->bodyInBHCell(b)) {
                return this->subcells[i];
            }
        }

        throw "Failed to find child which contains body";
    }

    void print() {
        std::cout << "Coordinates: (" << this->x << " " << this->y << ")" << std::endl;
        std::cout << "Width and heigth: (" << this->width << " " << this->height << ")" << std::endl;
        std::cout << "Bodies: " << std::endl;
        for (int i = 0; i < bodies.size(); ++i) {
            std::cout << "id: " << bodies[i].id;
            std::cout << " coords: " << bodies[i].x << " " << bodies[i].y << std::endl;
        }

        if(!this->isLeaf) {
            for (int i = 0; i < 4; ++i) {
                this->subcells[i]->print();
            }
        }
    }

    void simplePrint() {
        std::cout << "Coordinates: (" << this->x << " " << this->y << ")" << std::endl;
        std::cout << "Width and heigth: (" << this->width << " " << this->height << ")" << std::endl;
        std::cout << "Bodies: " << std::endl;
        for (int i = 0; i < bodies.size(); ++i) {
            std::cout << "id: " << bodies[i].id;
            std::cout << " coords: " << bodies[i].x << " " << bodies[i].y << std::endl;
        }
    }
};

std::vector<Body> bodyList;
std::vector<Body> myBodies;
std::vector<Body> myNewBodies;

/*
 *Orb tree uses the Orthogonal recursive bisection algorithm to distribute the data
 *between the processors.
 */
class OrbTree {
    OrbTree* left;
    OrbTree* right;
    MPI_Group processorsSubsetGroup;
    double minX, minY, maxX, maxY, splitCoordinate;
    bool isXSplit;
    int bit;

    //returns the ratio of the work done above
    //the split compared to the whole work
    double checkSplit(double split) {
        int workAbove = 0;
        int workBelow = 0;
        int globalWorkAbove = 0;
        int globalWorkBelow = 0;

        for (int i = 0; i < myBodies.size(); ++i) {
            if(is_above(myBodies[i], split, minX, minY, maxX, maxY, isXSplit)) {
                workAbove += myBodies[i].weight;
            } else if(is_below(myBodies[i], split, minX, minY, maxX, maxY, isXSplit)) {
                workBelow += myBodies[i].weight;
            }
        }


        if (MPI_Allreduce(&workAbove, &globalWorkAbove, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD) != MPI_SUCCESS) {
            throw "failed to reduce";   
        }

        if (MPI_Allreduce(&workBelow, &globalWorkBelow, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD) != MPI_SUCCESS) {
            throw "failed to reduce";   
        }

        return (double)globalWorkAbove/(globalWorkBelow + globalWorkAbove);
    }

    //With this method we are looking for a processor workRatio that is about 0.5.
    //This means that the work above the split and below the split is equal.
    double findOptimalSplit(double maxSplit, double splitStart) {

        double splitCoordinate = maxSplit / 2 + splitStart;
        double workRatio = 0, oldWorkRatio = 0, olderWorkRatio = 0;
        int i = 0;
        //The step is the amount by which we offset the split on each iteration
        double step = 0.1;
        int loopDetector = 0;

        //It is possible to enter an endless loop here. Where the accurasy
        //is just not close enough and for that reason I added a loopDetector
        while (std::abs(workRatio - 0.5) > ORB_SPLIT_ERROR && loopDetector < 100) {
            //Move the split coordinate up or down based on the workRatio
            //if the workRatio is greater than 0.5 it means that we need to 
            //move upward so less bodies are above the split and vice verca
            if (i != 0 && workRatio - 0.5 > 0 ) {
                splitCoordinate += step * maxSplit;
            } else if(i != 0) {
                splitCoordinate -= step * maxSplit;
            }


            i++;

            //Make the step smaller with time so we can get closer to the
            //desired workRatio. 
            if (i % 10 == 0 && step >= (double)1/pow(10, 3)) {
                step = step / 10;
            }

            //Increment the loopDetector when we have returned
            //to the same value that was in the second to last iteration.
            //This is when the workRatio swaps between the same two
            //values constantly. One where more work is below the split
            //and the other when it is above
            if (std::abs(olderWorkRatio - workRatio) < 0.001) {
                loopDetector++;
            } else {
                loopDetector = 0;
            }

            //recalculate work ratios
            olderWorkRatio = oldWorkRatio;
            oldWorkRatio = workRatio;
            workRatio = this->checkSplit(splitCoordinate);
        }

        return splitCoordinate;
    }

    //Divide the processors in such a way that you get a hypercube
    //The idea is that we want every split to act as a communication
    //channel with an assigned number/the bit. We use the XOR operator
    //to divide the processors such that they differ in a single bit
    //of their binary form. In this way every processor on one side of the
    //split has a processor on the other side with which it can communicate.
    std::vector<int> generate_left_group(int groupSize) {
        std::vector<int> left, binary;
        int world_rank;

        for (int i = 0; i < groupSize; ++i) {
            world_rank = getProcessorWorldRank(i, this->processorsSubsetGroup);
            std::vector<int> binary = getBinary(world_rank);

            //This check is taken from salmon
            if(binary[this->bit] ^ 1) {
                left.push_back(i);
            }
        }

        return left;
    }

    void addBodiesFromSectorToMyBodies(int worldRank) {
        int processorNumber;
        MPI_Comm_rank(MPI_COMM_WORLD, &processorNumber);

        if (processorNumber == worldRank) {
            myNewBodies.clear();

            for (int i = 0; i < bodyCount; ++i) {
                if(in_borders(bodyList[i], minX, minY, maxX, maxY)) {
                    myNewBodies.push_back(bodyList[i]);
                }
            }
        }
    }

    //Split the processors into two groups
    //The odd numbered ones in one and the even numbered ones
    //in the other. In a group all processors are id'd from 0 to
    //the number of processors in a group. You have to use a method
    //to translate the ranks to the global id's
    void divideProcessors(int groupSize, double splitCoordinate) {
        MPI_Group leftGroup, rightGroup;
        std::vector<int> left;

        left = this->generate_left_group(groupSize);

        if(MPI_Group_incl(processorsSubsetGroup, left.size(), left.data(), &leftGroup) != MPI_SUCCESS) {
            throw "Failed processor group creation include";
        }

        if(MPI_Group_excl(processorsSubsetGroup, left.size(), left.data(), &rightGroup) != MPI_SUCCESS) {
            throw "Failed processor group creation exclude";
        }


        if(this->isXSplit) {
            this->left = new OrbTree(leftGroup, minX, minY, splitCoordinate, maxY, this->bit + 1);
            this->right = new OrbTree(rightGroup, splitCoordinate, minY, maxX, maxY, this->bit + 1);

            this->left->split();
            this->right->split();
        } else {
            this->left = new OrbTree(leftGroup, minX, minY, maxX, splitCoordinate, this->bit + 1);
            this->right = new OrbTree(rightGroup, minX, splitCoordinate, maxX, maxY, this->bit + 1);
            this->left->split();
            this->right->split();
        }
    }

    void copyCoordinates(double minX, double minY, double maxX, double maxY, int bit) {
        this->minX = minX;
        this->minY = minY;
        this->maxX = maxX;
        this->maxY = maxY;
        this->bit = bit;
        this->left = nullptr;
        this->right = nullptr;
    }

    void destroy() {
        if (this->left != nullptr) {
            delete this->left;
            this->left = nullptr;
        }

        if (this->right != nullptr) {
            delete this->right;
            this->right = nullptr;
        }
    }

    void copy(OrbTree* left, OrbTree* right, double minX, double minY, double maxX,
            double maxY, double splitCoordinate, int bit, bool isXSplit, MPI_Group group) {

        this->splitCoordinate = splitCoordinate;
        this->copyCoordinates(minX, minY, maxX, maxY, bit);
        this->isXSplit = isXSplit;
        this->processorsSubsetGroup = group;

        if(left != nullptr) {
            this->left = new OrbTree(*left);
        } 

        if(right != nullptr) {
            this->right = new OrbTree(*right);
        }
    }

    public:
    OrbTree() {
        this->copyCoordinates(0,0, spaceX, spaceY, 0);

        this->left = nullptr;
        this->right = nullptr;
    }

    OrbTree(MPI_Comm comm, double minX, double minY, double maxX, double maxY, int bit = 0) {
        MPI_Comm_group(comm, &processorsSubsetGroup);
        this->copyCoordinates(minX, minY, maxX, maxY, bit);

        this->left = nullptr;
        this->right = nullptr;
    }

    OrbTree(MPI_Group group, double minX, double minY, double maxX, double maxY, int bit = 0) {
        this->processorsSubsetGroup = group;
        this->copyCoordinates(minX, minY, maxX, maxY, bit);

        this->left = nullptr;
        this->right = nullptr;
    }
    
    OrbTree(const OrbTree &other){
        this->copy(other.left, other.right, other.minX, other.minY, other.maxX, other.maxY,
                other.splitCoordinate, other.bit, other.isXSplit, other.processorsSubsetGroup);
    }

    ~OrbTree() {
        this->destroy();
    }


    OrbTree& operator=(const OrbTree &other) {
        if (this != &other) {
            this->destroy();
            this->copy(other.left, other.right, other.minX, other.minY, other.maxX, other.maxY,
                    other.splitCoordinate, other.bit, other.isXSplit, other.processorsSubsetGroup);
        }
    
        return *this;
    }

    double getSplitCoordinate() {
        return this->splitCoordinate;
    }

    double getIsXSplit() {
        return this->isXSplit;
    }

    int getBit() {
        return this->bit;
    }

    double getMinY() {
        return this->minY;
    }

    double getMaxY() {
        return this->maxY;
    }

    double getMinX() {
        return this->minX;
    }

    double getMaxX() {
        return this->maxX;
    }

    OrbTree* getLeft() {
        return this->left;
    }

    OrbTree* getRight() {
        return this->right;
    }

    //Split builds the ORB tree
    void split() {
        int groupSize;
        MPI_Group_size(processorsSubsetGroup, &groupSize);

        int process_Rank;
        MPI_Comm_rank(MPI_COMM_WORLD, &process_Rank);

        if(groupSize == 1) {
            int worldRank = getProcessorWorldRank(0, this->processorsSubsetGroup);
            this->addBodiesFromSectorToMyBodies(worldRank);
            
            return;
        }

        //Decide if we should split by x or by y.
        //We split be the longer coordinate. Based on Salmon's thesis
        //where it is said that by doing this we save memory
        double maxSplit;
        double splitStart;
        this->isXSplit = (maxX - minX >= maxY - minY);

        if(this->isXSplit) {
            maxSplit = maxX - minX;
            splitStart = minX;
        } else {
            maxSplit = maxY - minY;
            splitStart = minY;
        }

	
        auto start = std::chrono::high_resolution_clock::now();
        this->splitCoordinate = this->findOptimalSplit(maxSplit, splitStart);
        logTime(start, "time to find ORB split");

        this->divideProcessors(groupSize, this->splitCoordinate);
    }

    void print() {
        std::cout << "minX, minY: " << this->minX << " " << this->maxY 
            << " maxX, maxY: " << this->maxX << " " << this->maxY << std::endl;
        std::cout << "split: " << this->splitCoordinate << "isXSplit: " << this->isXSplit << std::endl;
        std::cout << "bit: " << this->bit << std::endl;
    }

    void printProcessors() {
        int groupSize;
        MPI_Group_size(processorsSubsetGroup, &groupSize);

        for (int i = 0; i < groupSize; ++i) {
            int worldRank = getProcessorWorldRank(i, processorsSubsetGroup);

            std::cout << worldRank << " ";
        }

        std::cout << std::endl;
    }

};

/*
 *Returns the ORBTree nodes which the current processor is a part of
 *This is done based on one of the elements of the processor.
 */
std::vector<OrbTree> getMySplits(Body body, OrbTree* root) {
    std::vector<OrbTree> result;
    result.push_back(*root);
    OrbTree* current = root;

    while(current != nullptr) {
        if (processorLeftOfSplit(body.x, body.y, current->getSplitCoordinate(), current->getIsXSplit()) 
                && current->getLeft() != nullptr) {

            result.push_back(*current->getLeft());
            current = current->getLeft();
        } else if (!processorLeftOfSplit(body.x, body.y, current->getSplitCoordinate(), current->getIsXSplit()) 
                && current->getRight() != nullptr) {
            result.push_back(*current->getRight());
            current = current->getRight();
        } else {
            current = nullptr;
        }
    }

    return result;
}

void createNodeChildren(BHCell* node) {
    node->isLeaf = false;
    double newNodeHeight = node->height/2;
    double newNodeWidth = node->width/2;
    //lower left coordinate of the new cells
    std::vector<std::pair<double, double>> newCoords;
    newCoords.push_back(std::pair<double, double>(node->x, node->y));
    newCoords.push_back(std::pair<double, double>(node->x + newNodeWidth, node->y));
    newCoords.push_back(std::pair<double, double>(node->x, node->y + newNodeHeight));
    newCoords.push_back(std::pair<double, double>(node->x + newNodeWidth, node->y + newNodeHeight));

    for(int i = 0; i < 4; i++) {
        node->subcells[i] = new BHCell(newCoords[i].first, newCoords[i].second,
                newNodeWidth, newNodeHeight, node->bodies);

        if(node->subcells[i]->bodies.size() > BODIES_PER_LEAF) {
            createNodeChildren(node->subcells[i]);
        }
    }
}

void addBody(BHCell* node, Body body) {
    node->bodies.push_back(body);

    if (node->isLeaf) {
        //causes segfault if I use &&. Should check why
        if (node->bodies.size() > BODIES_PER_LEAF) {
            createNodeChildren(node);
        }
    } else {
        BHCell* child = node->getChildForBody(body);
        addBody(child, body);
    }
}


double computeDistance(double x1, double y1, double x2, double y2) {
    return sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
}

/*
 *Calculates the distance between a segment given by the points (x1, y1), (x2, y2) and the point with
 *coordinates pointX and pointY
 */
double minDistanceToSegment(double x1, double y1, double x2, double y2, double pointX, double pointY) {
    double lengthSquared = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);
    if (lengthSquared == 0) {
        return computeDistance(x1, y1, pointX, pointY);
    }

    double dotProduct = (pointX - x1) * (x2 - x1) + (pointY - y1) * (y2 - y1);
    const float t = std::max(0.0, std::min(1.0, dotProduct / lengthSquared));
    
    double segmentX = x1 + t * (x2 - x1);
    double segmentY = y1 + t * (y2 - y1);

    return computeDistance(segmentX, segmentY, pointX, pointY);
}

/*
 *This criterion detirmines the distance between the 
 *cell and the split and says that if the dist * ACCURACY
 *is less than the width or hieght of the cell respectively
 *Then we must calculate the children. So if the criterion
 *is not true we can generalize.
 */
bool domainCriterion(BHCell* node, OrbTree split) {
    double dist;
    if(split.getIsXSplit()) {
        dist = minDistanceToSegment(split.getSplitCoordinate(), split.getMinY(),
            split.getSplitCoordinate(), split.getMaxY(), node->cmass_x, node->cmass_y);
        return dist * ACCURACY < node->width;
    } 

    dist = minDistanceToSegment(split.getMinX(), split.getSplitCoordinate(),
        split.getMaxX(), split.getSplitCoordinate(), node->cmass_x, node->cmass_y);

    return dist * ACCURACY < node->height;
}

void enqueLeafBodies(std::vector<MessageBody>& toSend, std::vector<Body> bodies) {
    for (int i = 0; i < bodies.size(); ++i) {
        toSend.push_back(MessageBody(bodies[i].x, bodies[i].y, bodies[i].mass));
    }
}

/*
 *This method finds the essential nodes for the other side of the splits and adds them
 *to toSend so they can be sent on the next step
 */
void findEssentialNodes(BHCell* node, std::vector<MessageBody>& toSend, OrbTree split) {
    if (node->bodies.size() <= 0) {
        return;
    }

    if (node->bodies.size() <= BODIES_PER_LEAF) {
        enqueLeafBodies(toSend, node->bodies);
    } else if (!domainCriterion(node, split)) {
        toSend.push_back(MessageBody(node->cmass_x, node->cmass_y, node->mass));
    } else {
        for (int i = 0; i < 4; ++i) {
            findEssentialNodes(node->subcells[i], toSend, split);
        }
    }
}

/*
 *Creates a barnes hut tree from the bodies that are in the bodies vector.
 *used mainly to build the local tree from myBodies
 */
void buildLocalTree(BHCell* root, std::vector<Body> bodies) {
    for (int i = 0; i < bodies.size(); ++i) {
        addBody(root, bodies[i]);
    }
}

void receiveData(int neighborRank, std::vector<MessageBody>& toAdd) {

    MPI_Status stat;
    MPI_Probe(neighborRank, 0, MPI_COMM_WORLD, &stat);

    int cnt;
    MPI_Get_count(&stat, TMPIMessageBody, &cnt);

    toAdd.resize(cnt);
    MPI_Recv(toAdd.data(), cnt, TMPIMessageBody,
            neighborRank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
}

void sendData(int neighborRank, std::vector<MessageBody> toSend) {
    MPI_Send(toSend.data(), toSend.size(), TMPIMessageBody, neighborRank, 0, MPI_COMM_WORLD);
}

/*
 *Exchanges data with it's neighbor on the other side of the split
 *This neighbor exists based on the way we divided the processor
 *during the ORB distribution.
 */
void exchangeNodesAndMerge(bool leftOfSplit, int neighborRank, std::vector<MessageBody> toSend, BHCell* root) {
    std::vector<MessageBody> toAdd;

    //The processors at the left of the split receive and then send
    //those to the right do the opposite so we avoid a deadlock
    if (leftOfSplit) {
        receiveData(neighborRank, toAdd);
        sendData(neighborRank, toSend);
    } else {
        sendData(neighborRank, toSend);
        receiveData(neighborRank, toAdd);
    }

    for (int i = 0; i < toAdd.size(); ++i) {
        addBody(root, Body(toAdd[i]));
    }
}

void computeCellForce(Body& body, double x, double y, double mass) {
    double dist = computeDistance(body.x, body.y, x, y);
    double force = GRAVITY * (body.mass * mass) / pow(dist, 2.0);

    //force direction
    body.forceX += force * ((x - body.x) / dist);
    body.forceY += force * ((y - body.y) / dist);
}


//Computes the forces acting on every body the processor is responsible for
void computeForces(BHCell* node, Body& body) {
    if(node->bodies.size() <= 0) {
        return;
    }

    if(node->isLeaf) {
        Body other = node->bodies[0];
        if(other.id != body.id) {
            computeCellForce(body, other.x, other.y, other.mass);
            body.weight++;
        }
    } else {
        double dist = computeDistance(body.x, body.y, node->cmass_x, node->cmass_y);

        if(dist * ACCURACY < node->width) {
            for (int i = 0; i < 4; ++i) {
                computeForces(node->subcells[i], body);
            }
        } else {
            computeCellForce(body, node->cmass_x, node->cmass_y, node->mass);
            body.weight++;
        }
    }
}

/*
 *We take the force and divide it by the mass which gives us the acceleration
 *which myltiplied by time gives is how the velocity changes
 */
void computeVelocity() {
    for (int i = 0; i < myBodies.size(); ++i) {
        myBodies[i].velocityX += (myBodies[i].forceX / myBodies[i].mass) * DELTAT;
        myBodies[i].velocityY += (myBodies[i].forceY / myBodies[i].mass) * DELTAT;
    }
}

void updatePositions() {
    for (int i = 0; i < myBodies.size(); ++i) {
        myBodies[i].x += myBodies[i].velocityX * DELTAT;
        myBodies[i].y += myBodies[i].velocityY * DELTAT;

        if(myBodies[i].x < 0) {
            myBodies[i].x = 0;
            myBodies[i].velocityX *= -1;
        } else if(myBodies[i].x > spaceX) {
            myBodies[i].x = spaceX;
            myBodies[i].velocityX *= -1;
        }

        if(myBodies[i].y < 0) {
            myBodies[i].y = 0;
            myBodies[i].velocityY *= -1;
        } else if(myBodies[i].y > spaceY) {
            myBodies[i].y = spaceY;
            myBodies[i].velocityY *= -1;
        }
    }
}


void printMyBodiesCoordinates(int process_Rank) {
    if (process_Rank == MASTER) {
        for (int i = 0; i < myBodies.size(); ++i) {
            std::cout << "id: " << myBodies[i].id << " coordinates: " << myBodies[i].x << "," << myBodies[i].y << std::endl;
        }
    }
}

void printBodyListCoordinates(int process_Rank) {
    for (int i = 0; i < bodyList.size(); ++i) {
        std::cout << "id: " << bodyList[i].id << " coordinates: " << bodyList[i].x << "," << bodyList[i].y << std::endl;
    }
}

//Synchronizes the bodyList after all myBodies have been updated
void synchronizeBodies() {
    int size_Of_Cluster;
    MPI_Comm_size(MPI_COMM_WORLD, &size_Of_Cluster);
    int bodyCountForProcessor[size_Of_Cluster];
    int nbodies = myBodies.size();

    MPI_Allgather(&nbodies, 1, MPI_INT, bodyCountForProcessor, 1, MPI_INT, MPI_COMM_WORLD);

    int displacements[size_Of_Cluster];
    for (int i = 0; i < size_Of_Cluster; i++) {
        displacements[i] = (i > 0) ? (displacements[i - 1] + bodyCountForProcessor[i - 1]) : 0;
    }

    MPI_Allgatherv(myBodies.data(), myBodies.size(), TMPIBody, 
            bodyList.data(), bodyCountForProcessor, displacements, TMPIBody, MPI_COMM_WORLD); 
}

/*
 *Adds the nodes to the tree required for the calculations
 *of it's own bodies.
 */
void buildLocallyEssentialTree(OrbTree rootORB) {
    BHCell rootBH;
    bool leftOfSplit;
    int neighborRank;

    auto start = std::chrono::high_resolution_clock::now();

    buildLocalTree(&rootBH, myBodies);
    std::vector<OrbTree> splits = getMySplits(myBodies[0], &rootORB);

    logTime(start, "Local tree build and local splits get time");

    auto startFor = std::chrono::high_resolution_clock::now();
    //-1 because the last split contains only our area
    for (int i = 0; i < splits.size() - 1; ++i) {
        std::vector<MessageBody> toSend;
        start = std::chrono::high_resolution_clock::now();
        //recalculate metrics when new bodies have been added
        rootBH.calculateMetrics();
        logTime(start, " Time for tree metrics recalculation");

        start = std::chrono::high_resolution_clock::now();
        findEssentialNodes(&rootBH, toSend, splits[i]);
        leftOfSplit = processorLeftOfSplit(myBodies[0].x, myBodies[0].y,
                splits[i].getSplitCoordinate(), splits[i].getIsXSplit());
        neighborRank = getNeighborFromBit(splits[i].getBit());

        logTime(start, "find toSend");

        //MPI_Barrier(MPI_COMM_WORLD);
        start = std::chrono::high_resolution_clock::now();
        exchangeNodesAndMerge(leftOfSplit, neighborRank, toSend, &rootBH);

        logTime(start, " exchange bodies");
    }

    logTime(startFor, "TIME Locally Essential For");

    int process_Rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &process_Rank);

    start = std::chrono::high_resolution_clock::now();
    std::cout << "SIZE " << myBodies.size() << " rank:" << process_Rank << std::endl;

    for (int i = 0; i < myBodies.size(); ++i) {
        myBodies[i].weight = 0;
        myBodies[i].forceX = 0;
        myBodies[i].forceY = 0;
        computeForces(&rootBH, myBodies[i]);
    }

    computeVelocity();
    updatePositions();

    logTime(start, "Calculations time");
}

void runSimulation(OrbTree root) {
    int process_Rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &process_Rank);

    for (int i = 0; i < TIME; ++i) {
        auto start = std::chrono::high_resolution_clock::now();

        buildLocallyEssentialTree(root);
        logTime(start, "Build locally essential tree time");

        //MPI_Barrier(MPI_COMM_WORLD);
        start = std::chrono::high_resolution_clock::now();

        synchronizeBodies();
        logTime(start, "time for synchronization of processor bodies");

        //MPI_Barrier(MPI_COMM_WORLD);
        //if(process_Rank == MASTER) {
        //    std::cout << "--------------------" << i << std::endl;
        //    printBodyListCoordinates(0);
        //    std::cout << "--------------------" << i << std::endl;
        //}

        start = std::chrono::high_resolution_clock::now();
        myNewBodies.clear();
        root = OrbTree(MPI_COMM_WORLD, 0, 0, spaceX, spaceY);
        root.split();

        myBodies.clear();
        myBodies.insert(myBodies.begin(), myNewBodies.begin(), myNewBodies.end());

        logTime(start, "ORB build time");
    }
}

double generate_random_double() {
    return (rand() + 1) / ((double)RAND_MAX + 1);
}

void initialize_bodies() {
    int maxX = spaceX - MAX_RADIUS;
    int maxY = spaceY - MAX_RADIUS;

    for (int i = 0; i < bodyCount; ++i) {
        Body newBody;
        newBody.id = i;
        newBody.weight = 1;
        newBody.x = generate_random_double() * maxX;
        newBody.y = generate_random_double() * maxY;
        newBody.mass = generate_random_double() * MAX_MASS;
        newBody.radius = generate_random_double() * MAX_RADIUS;
        newBody.velocityX = generate_random_double() * MAX_VELOCITY ;
        newBody.velocityY = generate_random_double() * MAX_VELOCITY ;
        bodyList.push_back(newBody);
    }
}

void printBodyListId(int process_Rank) {
    for (int i = 0; i < bodyList.size(); ++i) {
        std::cout << bodyList[i].id << " rank:" << process_Rank << std::endl;
    }
}

void buildTypes() {
    //Body type used for transimitting the body structure
    MPI_Aint displacements[2] = {offsetof(Body, id), offsetof(Body, x)};
    int block_lengths[2] = {2, 8};
    MPI_Datatype types[2] = {MPI_INT, MPI_DOUBLE};

    MPI_Type_create_struct(2, block_lengths, displacements, types, &TMPIBody); 
    MPI_Type_commit(&TMPIBody);

    //Create message body type for MPI
    MPI_Type_contiguous(3, MPI_DOUBLE, &TMPIMessageBody);
    MPI_Type_commit(&TMPIMessageBody);
}

int main(int argc, char *argv[]) {

    int process_Rank, size_Of_Cluster;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size_Of_Cluster);
    MPI_Comm_rank(MPI_COMM_WORLD, &process_Rank);

    if (process_Rank == 0) {
        srand(time(NULL));
        initialize_bodies();
    } else {
        bodyList.resize(bodyCount);
    }

    myBodies.resize(bodyCount/size_Of_Cluster); 

    buildTypes();

    MPI_Bcast(bodyList.data(), bodyCount, TMPIBody, MASTER, MPI_COMM_WORLD);
    //mod might cause for more than one processor to be left out
    MPI_Scatter(bodyList.data(), bodyCount/size_Of_Cluster, TMPIBody,
                myBodies.data(), bodyCount/size_Of_Cluster, TMPIBody, MASTER, MPI_COMM_WORLD);

    OrbTree rootORB(MPI_COMM_WORLD, 0, 0, spaceX, spaceY);

    auto start = std::chrono::high_resolution_clock::now();

    rootORB.split();

    myBodies.clear();
    myBodies.insert(myBodies.begin(), myNewBodies.begin(), myNewBodies.end());

    runSimulation(rootORB);

    logTime(start, "Total time");

    MPI_Finalize();
    return 0;

}
