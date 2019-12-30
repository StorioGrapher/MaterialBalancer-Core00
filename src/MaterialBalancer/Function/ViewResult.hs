module MaterialBalancer.Function.ViewResult where


import qualified Data.IntMap                   as I
import           Data.Text                      ( Text )
import qualified Data.Vector                   as V

import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis
import           MaterialBalancer.Data.Column
import           MaterialBalancer.Data.ValueStorage
import           MaterialBalancer.Data.TheTable
import           MaterialBalancer.Data.View
import           MaterialBalancer.Data.ViewResult

buildResultSimple2D :: View2D -> TheTable -> ViewResultSimple2D
buildResultSimple2D View2D {..} (am, csm, vs) = V.fromList (targetCT : results)
 where
  targetCM = getColumnMap targetAxis2D csm
  -- NOTE: When VGroup have no Labeling (Nothing), then there is no label
  -- NOTE: When vcList have no Labeling (Nothing), then get label text from targetCM
  targetCT = V.fromList $ concatMap (processVG targetCM) viewSetting2D
   where
    processVG cm (V2DGroup gLabeling vcList) = header ++ rest
     where
      header = VCSDelimiter : maybe [] (\l -> [VCSLabel l]) gLabeling
      rest   = map
        (\(cLabeling, ci) -> maybe (VCSColumn $ cm I.! ci) VCSColumn cLabeling)
        vcList
  results = map processVRS baseColumns2D
   where
    -- NOTE: based on given baseColumnIndex and targetColumnIndex from List, generate a ViewResultSimple
    processVRS bCI = V.fromList $ concatMap processVG viewSetting2D
     where
      processVG (V2DGroup gLabeling vcList) = header ++ rest
       where
        header = VCSDelimiter : maybe [] (\l -> [VCSLabel l]) gLabeling
        rest   = map processVC vcList
        processVC (_, tCI) =
          VCSVariable
            . getVariable [(baseAxis2D, bCI), (targetAxis2D, tCI)]
            $ vs
